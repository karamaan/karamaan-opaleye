{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, Rank2Types,
             FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}

module Karamaan.Opaleye.RunQuery where

import Karamaan.Opaleye.Wire (Wire)
import Karamaan.Opaleye.Unpackspec (Unpackspec)
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.Internal (RowParser)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Data.Profunctor (Profunctor, dimap)
import Data.Functor.Contravariant (Contravariant, contramap)
import Data.Profunctor.Product (ProductProfunctor, empty, (***!),
                                point, (***<))
import Control.Applicative (pure, liftA2)
import Karamaan.Opaleye.Default (Default, def)
import Karamaan.Opaleye.Colspec (writerWire)
import Karamaan.Opaleye.Unpackspec (Unpackspec(Unpackspec))
import Karamaan.Opaleye.SQL (showSqlForPostgreSQLSimple,
                             showSqlForPostgreSQLSimpleUnopt)
import Karamaan.Opaleye.QueryArr (Query)
import Data.String (fromString)

import Data.Reflection (Reifies, reflect, reify)
import Data.Proxy (Proxy(Proxy))

import Data.Time.Calendar (Day)

import qualified Karamaan.WhaleUtil.Database as UD

data QueryRunner a b = QueryRunner (Unpackspec a) (RowParser b)

instance Profunctor QueryRunner where
  dimap f g (QueryRunner u b) = QueryRunner (contramap f u) (fmap g b)

instance ProductProfunctor QueryRunner where
  empty = QueryRunner point (pure ())
  QueryRunner u b ***! QueryRunner u' b' =
    QueryRunner (u ***< u') (liftA2 (,) b b')

-- TODO: May want to make this "(Wire b) a" in the future
fieldQueryRunner :: FromField a => QueryRunner (Wire a) a
fieldQueryRunner = QueryRunner (Unpackspec writerWire) field

instance Default QueryRunner (Wire Int) Int where
  def = fieldQueryRunner

instance Default QueryRunner (Wire String) String where
  def = fieldQueryRunner

instance Default QueryRunner (Wire Double) Double where
  def = fieldQueryRunner

instance Default QueryRunner (Wire Day) Day where
  def = fieldQueryRunner

instance Default QueryRunner (Wire Bool) Bool where
  def = fieldQueryRunner

instance Default QueryRunner (Wire (Maybe Double)) (Maybe Double) where
  def = fieldQueryRunner

instance Default QueryRunner (Wire (Maybe Int)) (Maybe Int) where
  def = fieldQueryRunner

-- Reflection stuff, see
-- https://github.com/ekmett/reflection/blob/master/examples/Monoid.hs
newtype FR a s = FR { runFR :: a }

instance Reifies s (RowParser a) => FromRow (FR a s) where
  fromRow = fmap FR (reflect (Proxy :: Proxy s))

asProxyOf3 :: h (g (f s)) -> Proxy s -> h (g (f s))
asProxyOf3 a _ = a

runQuery :: QueryRunner a b -> Query a -> SQL.Connection -> IO [b]
runQuery (QueryRunner u rowParser) q conn = query_ rowParser conn sql
  where sql :: SQL.Query
        sql = fromString (showSqlForPostgreSQLSimple u q)

runQueryDefault :: Default QueryRunner a b => Query a -> IO [b]
runQueryDefault q = do
  conn <- SQL.connect UD.connectInfo
  runQuery def q conn

-- SQL.query_ with explicit RowParser
--
-- This uses Data.Reflection, and as such is very mysterious to me, so BE
-- CAREFUL!  Data.Reflection ought to be a known Haskell quantity, albeit
-- a powerful one, however no one really seems to understand this well
-- apart from Oleg and Edward Kmett.
query_ :: RowParser c -> SQL.Connection -> SQL.Query -> IO [c]
query_ rowParser conn sql = reify rowParser (fmap (map runFR)
                                            . asProxyOf3 (SQL.query_ conn sql))
