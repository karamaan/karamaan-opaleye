{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, Rank2Types,
             FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}

module Karamaan.Opaleye.RunQuery where

import Karamaan.Opaleye.Wire (Wire)
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.Internal (RowParser)
import Database.PostgreSQL.Simple.FromField (FieldParser, FromField, fromField)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, fieldWith)
import Data.Profunctor (Profunctor, dimap)
import Data.Functor.Contravariant (contramap)
import Data.Profunctor.Product (ProductProfunctor, empty, (***!),
                                defaultEmpty, defaultProfunctorProduct)
import Control.Applicative (Applicative, (<*>), pure)
import Data.Monoid ((<>), mempty)
import Data.Profunctor.Product.Default (Default, def)
import Karamaan.Opaleye.QueryColspec (writerWire)
import Karamaan.Opaleye.Unpackspec (Unpackspec(Unpackspec))
import Karamaan.Opaleye.SQL (showSqlForPostgres)
import Karamaan.Opaleye.QueryArr (Query)
import Data.String (fromString)
import Data.Text (Text)
import Data.UUID (UUID)

import Data.Reflection (Reifies, reflect, reify)
import Data.Proxy (Proxy(Proxy))

import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (LocalTime)
import Data.Time.Calendar (Day)

-- How to define new 'QueryRunner's for your own datatypes:
--
-- You should probably just do (if you're wrapping an Int with MyType say)
--
-- newtype MyType = MyType Int
--
-- instance Default QueryRunner (Wire MyType) MyType where
--     def = fieldQueryRunnerF MyType

-- * 'wires' represents types like '(Wire Int, Wire Bool, Wire String)'
-- * 'haskells' represents types like '(Int, Bool, String)'
data QueryRunner wires haskells = QueryRunner (Unpackspec wires)
                                              (RowParser haskells)

instance Profunctor QueryRunner where
  dimap f g (QueryRunner u b) = QueryRunner (contramap f u) (fmap g b)

instance Functor (QueryRunner a) where
  fmap f (QueryRunner u r) = QueryRunner u (fmap f r)

instance Applicative (QueryRunner a) where
  pure = QueryRunner mempty . pure
  QueryRunner u r <*> QueryRunner u' r' = QueryRunner (u <> u') (r <*> r')

instance ProductProfunctor QueryRunner where
  empty = defaultEmpty
  (***!) = defaultProfunctorProduct

fieldQueryRunner :: FromField a => QueryRunner (Wire a) a
fieldQueryRunner = fieldQueryRunnerF id

fieldQueryRunnerF :: FromField a => (a -> b) -> QueryRunner (Wire b) b
fieldQueryRunnerF = fieldQueryRunnerUnclassed . flip fmapFieldParser fromField

fmapFieldParser :: (a -> b) -> FieldParser a -> FieldParser b
fmapFieldParser = fmap . fmap . fmap

-- TODO: May want to make this "(Wire b) a" in the future
-- TODO: put 'Unpackspec writerWire' in Unpackspec.hs
fieldQueryRunnerUnclassed :: FieldParser a -> QueryRunner (Wire a) a
fieldQueryRunnerUnclassed = QueryRunner (Unpackspec writerWire) . fieldWith

instance Default QueryRunner (Wire Int) Int where
  def = fieldQueryRunner

instance Default QueryRunner (Wire String) String where
  def = fieldQueryRunner

instance Default QueryRunner (Wire Text) Text where
  def = fieldQueryRunner

instance Default QueryRunner (Wire Double) Double where
  def = fieldQueryRunner

instance Default QueryRunner (Wire Day) Day where
  def = fieldQueryRunner

instance Default QueryRunner (Wire UTCTime) UTCTime where
  def = fieldQueryRunner

instance Default QueryRunner (Wire LocalTime) LocalTime where
  def = fieldQueryRunner

instance Default QueryRunner (Wire Bool) Bool where
  def = fieldQueryRunner

instance Default QueryRunner (Wire UUID) UUID where
  def = fieldQueryRunner

instance Default QueryRunner (Wire (Maybe Int)) (Maybe Int) where
  def = fieldQueryRunner

instance Default QueryRunner (Wire (Maybe String)) (Maybe String) where
  def = fieldQueryRunner

instance Default QueryRunner (Wire (Maybe Text)) (Maybe Text) where
  def = fieldQueryRunner

instance Default QueryRunner (Wire (Maybe Double)) (Maybe Double) where
  def = fieldQueryRunner

instance Default QueryRunner (Wire (Maybe Day)) (Maybe Day) where
  def = fieldQueryRunner

instance Default QueryRunner (Wire (Maybe UTCTime)) (Maybe UTCTime) where
  def = fieldQueryRunner

instance Default QueryRunner (Wire (Maybe LocalTime)) (Maybe LocalTime) where
  def = fieldQueryRunner

instance Default QueryRunner (Wire (Maybe Bool)) (Maybe Bool) where
  def = fieldQueryRunner

instance Default QueryRunner (Wire (Maybe UUID)) (Maybe UUID) where
  def = fieldQueryRunner

-- Reflection stuff, see
-- https://github.com/ekmett/reflection/blob/master/examples/Monoid.hs
newtype FR a s = FR { runFR :: a }

instance Reifies s (RowParser a) => FromRow (FR a s) where
  fromRow = fmap FR (reflect (Proxy :: Proxy s))

asProxyOf3 :: h (g (f s)) -> Proxy s -> h (g (f s))
asProxyOf3 a _ = a

runQuery :: QueryRunner wires haskells -> Query wires -> SQL.Connection
         -> IO [haskells]
runQuery (QueryRunner u rowParser) q conn = query_ rowParser conn sql
  where sql :: SQL.Query
        sql = fromString (showSqlForPostgres u q)

runQueryDefault :: Default QueryRunner wires haskells
                => Query wires -> SQL.Connection -> IO [haskells]
runQueryDefault = runQuery def

runQueryDefaultConnectInfo :: Default QueryRunner wires haskells
                              => SQL.ConnectInfo -> Query wires -> IO [haskells]
runQueryDefaultConnectInfo connectInfo q = do
  conn <- SQL.connect connectInfo
  runQueryDefault q conn

-- SQL.query_ with explicit RowParser
--
-- TODO: postgresql-simple now has queryWith_ which replaces this
--
-- This uses Data.Reflection, and as such is very mysterious to me, so BE
-- CAREFUL!  Data.Reflection ought to be a known Haskell quantity, albeit
-- a powerful one, however no one really seems to understand this well
-- apart from Oleg and Edward Kmett.
--
-- Wanted to use a "manual dictionary" or "free instance" along the
-- lines of "But what about manual dictionaries?" in
-- https://www.fpcomplete.com/user/thoughtpolice/using-reflection but
-- I couldn't see how to get it to work.  It seems we need to come up
-- with a value of type 'RowParser (RowParser c -> c)'.
query_ :: RowParser haskells -> SQL.Connection -> SQL.Query -> IO [haskells]
query_ rowParser conn sql = reify rowParser (fmap (map runFR)
                                            . asProxyOf3 (SQL.query_ conn sql))
