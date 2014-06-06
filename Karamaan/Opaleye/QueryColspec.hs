{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Karamaan.Opaleye.QueryColspec where

import Karamaan.Opaleye.Wire (Wire(Wire), unWire)
import Control.Applicative (Applicative, (<*>), pure, liftA2)
import Data.Functor.Contravariant (Contravariant, contramap)
import Data.Profunctor (Profunctor, dimap, lmap)
import Data.Profunctor.Product (ProductProfunctor, empty, (***!),
                                ProductContravariant, point, (***<),
                                defaultEmpty, defaultProfunctorProduct,
                                defaultPoint, defaultContravariantProduct)
import Data.Monoid (Monoid, mempty, mappend, (<>))
import Data.Profunctor.Product.Default (Default, def)

-- The constructor is called Writer as a historical accident.
-- TODO: need to think about naming a lot in this library
newtype MWriter m a = Writer (a -> m)

instance Contravariant (MWriter m) where
  contramap f (Writer w) = Writer (lmap f w)

instance Monoid m => Monoid (MWriter m a) where
  mempty = Writer mempty
  w `mappend` w' = Writer (runWriter w <> runWriter w')

instance Monoid m => ProductContravariant (MWriter m) where
  point = defaultPoint
  (***<) = defaultContravariantProduct

type LWriter e = MWriter [e]
type Writer = LWriter String

newtype PackMap a b = PackMap ((String -> String) -> a -> b)

runPackMap :: PackMap a b -> (String -> String) -> a -> b
runPackMap (PackMap p) = p

instance Profunctor PackMap where
  dimap f g (PackMap p) = PackMap (fmap (dimap f g) p)

instance Functor (PackMap a) where
  fmap f (PackMap p) = PackMap (fmap (fmap f) p)

instance Applicative (PackMap a) where
  pure = PackMap . const . const
  PackMap pf <*> PackMap px = PackMap (liftA2 (<*>) pf px)

instance ProductProfunctor PackMap where
  empty = defaultEmpty
  (***!) = defaultProfunctorProduct

runWriter :: MWriter m t -> t -> m
runWriter (Writer w) = w

writer :: (t -> m) -> MWriter m t
writer = Writer

-- Need this to have different argument and return type
-- for aggregatorMaker', which can change types (when using the
-- count aggregator)
packMapWire :: PackMap (Wire a) (Wire b)
packMapWire = PackMap (\f -> Wire . f . unWire)

writerWire :: Writer (Wire a)
writerWire = Writer (return . unWire)

queryColspecWire :: QueryColspec (Wire a) (Wire a)
queryColspecWire = QueryColspec writerWire packMapWire

-- A QueryColspec provides a way to extract a list of all the column
-- names occurring in a product type of Wires (using the Writer) and a
-- way of modifying the column names in place (using the PackMap).
--
-- We use QueryColspec in several places where we want to generate new
-- unique names from the names in existing queries.
data QueryColspec a b = QueryColspec (Writer a) (PackMap a b)

instance Functor (QueryColspec a) where
  fmap f (QueryColspec w p) = QueryColspec w (fmap f p)

instance Applicative (QueryColspec a) where
  pure = QueryColspec mempty . pure
  QueryColspec w pf <*> QueryColspec w' px = QueryColspec (w <> w') (pf <*> px)

instance Profunctor QueryColspec where
  dimap f g (QueryColspec w p) = QueryColspec (contramap f w) (dimap f g p)

instance Default QueryColspec (Wire a) (Wire a) where
  def = queryColspecWire

instance ProductProfunctor QueryColspec where
  empty = QueryColspec point empty
  (QueryColspec w p) ***! (QueryColspec w' p') =
    QueryColspec (w ***< w') (p ***! p')

runWriterOfQueryColspec :: QueryColspec a b -> a -> [String]
runWriterOfQueryColspec (QueryColspec f _) = runWriter f

runPackMapOfQueryColspec :: QueryColspec a b -> (String -> String) -> a -> b
runPackMapOfQueryColspec (QueryColspec _ p) = runPackMap p
