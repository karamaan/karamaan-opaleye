{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Karamaan.Opaleye.QueryColspec where

import Karamaan.Opaleye.Wire (Wire(Wire), unWire)
import Control.Arrow ((***))
import Control.Applicative (Applicative, (<$>), (<*>))
import Data.Functor.Contravariant (Contravariant, contramap)
import Data.Profunctor (Profunctor, dimap)
import Data.Profunctor.Product (ProductProfunctor, empty, (***!),
                                ProductContravariant, point, (***<))
import Data.Monoid (Monoid, mempty, (<>))
import Karamaan.Opaleye.Default (Default, def)

-- The constructor is called Writer as a historical accident.
-- TODO: need to think about naming a lot in this library
newtype MWriter m a = Writer (a -> m)

instance Contravariant (MWriter m) where
  contramap f (Writer w) = Writer (w . f)

instance Monoid m => ProductContravariant (MWriter m) where
  point = Writer (const mempty)
  w ***< w' = writer (uncurry (<>) . (runWriter w *** runWriter w'))

type LWriter e = MWriter [e]
type Writer = LWriter String

data PackMap a b = PackMap ((String -> String) -> a -> b)

runPackMap :: PackMap a b -> (String -> String) -> a -> b
runPackMap (PackMap p) = p

instance Profunctor PackMap where
  dimap f g (PackMap p) = PackMap (\s -> g . p s . f)

instance ProductProfunctor PackMap where
  empty = PackMap (\_ -> id)
  (PackMap p) ***! (PackMap p') = PackMap ((***) <$> p <*> p')

runWriter :: MWriter m t -> t -> m
runWriter (Writer w) x = w x

writer :: (t -> m) -> MWriter m t
writer = Writer

-- Need this to have different argument and return type
-- for aggregatorMaker', which can change types (when using the
-- count aggregator)
packMapWire :: PackMap (Wire a) (Wire b)
packMapWire = PackMap (\f -> Wire . f . unWire)

writerWire :: Writer (Wire a)
writerWire = Writer (return . unWire)

data QueryColspec a b = QueryColspec (Writer a) (PackMap a b)

instance Profunctor QueryColspec where
  dimap f g (QueryColspec w p) = QueryColspec (contramap f w) (dimap f g p)

instance Default QueryColspec (Wire a) (Wire a) where
  def = QueryColspec writerWire packMapWire

instance ProductProfunctor QueryColspec where
  empty = QueryColspec point empty
  (QueryColspec w p) ***! (QueryColspec w' p') =
    QueryColspec (w ***< w') (p ***! p')

runWriterOfQueryColspec :: QueryColspec a b -> a -> [String]
runWriterOfQueryColspec (QueryColspec f _) = runWriter f

runPackMapOfQueryColspec :: QueryColspec a b -> (String -> String) -> a -> b
runPackMapOfQueryColspec (QueryColspec _ p) = runPackMap p
