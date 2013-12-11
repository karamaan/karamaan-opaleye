{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Karamaan.Opaleye.TableColspec where

import Karamaan.Opaleye.Wire (Wire(Wire), unWire)
import Karamaan.Opaleye.QueryColspec (QueryColspec(QueryColspec),
                                      runWriterOfQueryColspec,
                                      runPackMapOfQueryColspec,
                                      MWriter(Writer),
                                      PackMap(PackMap))
import Control.Applicative (Applicative, pure, (<*>))
import Data.Monoid (Monoid, mempty)
import Data.Profunctor.Product.Default (Default, def)
import Data.Profunctor (Profunctor, dimap)
import Data.Profunctor.Product (ProductProfunctor, empty, (***!),
                                defaultEmpty, defaultProfunctorProduct)

-- TODO: this happens to have the same implementation as QueryColspec, but I
-- don't want to suggest that one derives from the other.  Perhaps make this
-- clearer by introducing another type from which they both inherit their
-- implementation.
newtype TableColspecP a b = TableColspecP (QueryColspec a b)

-- TODO: we don't actually need TableColspec anymore.  It's just a partially
-- applied TableColspecP.  We should unpick its usage from makeTable replacing
-- it with TableColspecP, and then delete its definition.
data TableColspec a = TableColspec [String] ((String -> String) -> a)

tableColspecOfTableColspecP :: TableColspecP a b -> a -> TableColspec b
tableColspecOfTableColspecP (TableColspecP q) a =
  TableColspec (runWriterOfQueryColspec q a)
               (\b -> runPackMapOfQueryColspec q b a)

instance Functor TableColspec where
  fmap f (TableColspec s m) = TableColspec s (f . m)

instance Applicative TableColspec where
  pure = TableColspec mempty . pure
  TableColspec s mf <*> TableColspec s' m = TableColspec (s ++ s') (mf <*> m)

instance Profunctor TableColspecP where
  dimap f g (TableColspecP q) = TableColspecP (dimap f g q)

instance ProductProfunctor TableColspecP where
  empty = TableColspecP empty
  TableColspecP q ***! TableColspecP q' = TableColspecP (q ***! q')

instance Default TableColspecP (Wire a) (Wire a) where
  def = TableColspecP (QueryColspec (Writer (return . unWire))
                                    (PackMap (\f -> Wire . f . unWire)))

runWriterOfColspec :: TableColspec a -> [String]
runWriterOfColspec (TableColspec s _) = s

runPackMapOfColspec :: TableColspec a -> (String -> String) -> a
runPackMapOfColspec (TableColspec _ m) = m

colspec :: [String] -> ((String -> String) -> a) -> TableColspec a
colspec = TableColspec

col :: String -> TableColspec (Wire a)
col s = colspec [s] (\f -> Wire (f s))

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
  def = WireMaker Wire
