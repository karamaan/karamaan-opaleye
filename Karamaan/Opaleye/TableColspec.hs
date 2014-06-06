{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Karamaan.Opaleye.TableColspec where

import Karamaan.Opaleye.Wire (Wire(Wire))
import Karamaan.Opaleye.QueryColspec (QueryColspec(QueryColspec),
                                      runWriterOfQueryColspec,
                                      runPackMapOfQueryColspec,
                                      MWriter(Writer),
                                      PackMap(PackMap))
import Control.Applicative (Applicative, pure, (<*>))
import Data.Profunctor.Product.Default (Default, def)
import Data.Profunctor (Profunctor, dimap, lmap, rmap)
import Data.Profunctor.Product (ProductProfunctor, empty, (***!),
                                defaultEmpty, defaultProfunctorProduct)

-- TODO: this happens to have the same implementation as QueryColspec,
-- but I don't want to suggest that one derives from the other.
-- Perhaps make this clearer by introducing another type from which
-- they both inherit their implementation.  (Later note: is this
-- actually right?  Do we really want their behaviour to differ?).
newtype TableColspecP a b = TableColspecP (QueryColspec a b)

-- TODO: we don't actually need TableColspec anymore.  It's just a partially
-- applied TableColspecP.  We should unpick its usage from makeTable replacing
-- it with TableColspecP, and then delete its definition.
newtype TableColspec b = TableColspec (TableColspecP () b)

tableColspecOfTableColspecP :: TableColspecP a b -> a -> TableColspec b
tableColspecOfTableColspecP q a = TableColspec (lmap (const a) q)

instance Functor (TableColspecP a) where
  fmap f (TableColspecP c) = TableColspecP (fmap f c)

instance Applicative (TableColspecP a) where
  pure = TableColspecP . pure
  TableColspecP f <*> TableColspecP x = TableColspecP (f <*> x)

instance Functor TableColspec where
  fmap f (TableColspec c) = TableColspec (rmap f c)

instance Applicative TableColspec where
  pure = TableColspec . pure
  TableColspec f <*> TableColspec x = TableColspec (f <*> x)

instance Profunctor TableColspecP where
  dimap f g (TableColspecP q) = TableColspecP (dimap f g q)

instance ProductProfunctor TableColspecP where
  empty = defaultEmpty
  (***!) = defaultProfunctorProduct

instance Default TableColspecP (Wire a) (Wire a) where
  def = TableColspecP def

runWriterOfColspec :: TableColspec a -> [String]
runWriterOfColspec (TableColspec (TableColspecP c)) =
  runWriterOfQueryColspec c ()

runPackMapOfColspec :: TableColspec a -> (String -> String) -> a
runPackMapOfColspec (TableColspec (TableColspecP c)) f =
  runPackMapOfQueryColspec c f ()

-- TODO: this implementation is verbose
colspec :: [String] -> ((String -> String) -> a) -> TableColspec a
colspec w p = TableColspec
              (TableColspecP
               (QueryColspec (Writer (const w)) (PackMap (\f () -> p f))))

col :: String -> TableColspec (Wire a)
col s = colspec [s] (\f -> Wire (f s))

-- WireMaker is for turning a product container of column names (of
-- type String) into a product container of Opaleye Wires ('Wire a'
-- for any type 'a').
--
-- The basic value of this product profunctor is
--
--     wireCol :: WireMaker String (Wire a)
--
-- and generalising to products gives values like
--
--    WireMaker (String, (String, String, String))
--              (Wire Int, (Wire Bool, Wire Double, Wire String))
newtype WireMaker a b = WireMaker (a -> b)

runWireMaker :: WireMaker a b -> a -> b
runWireMaker (WireMaker f) = f

wireCol :: WireMaker String (Wire a)
wireCol = WireMaker Wire

instance Functor (WireMaker a) where
  fmap f (WireMaker g) = WireMaker (fmap f g)

instance Applicative (WireMaker a) where
  pure = WireMaker . pure
  WireMaker f <*> WireMaker x = WireMaker (f <*> x)

instance Profunctor WireMaker where
  dimap f g (WireMaker q) = WireMaker (dimap f g q)

instance ProductProfunctor WireMaker where
  empty = defaultEmpty
  (***!) = defaultProfunctorProduct

instance Default WireMaker String (Wire a) where
  def = wireCol
