module Karamaan.Opaleye.Colspec where

import Karamaan.Opaleye.Wire (Wire(Wire), unWire)
import Control.Arrow ((***))
import Control.Applicative (Applicative, pure, (<$>), (<*>))
import Data.Functor.Contravariant (Contravariant, contramap)
import Data.Profunctor (Profunctor, dimap)
import Karamaan.Opaleye.ProductProfunctor (ProductProfunctor, empty, (***!),
                                           ProductContravariant, point, (***<))
import Data.Monoid (mempty)

newtype Writer a = Writer (a -> [String])

instance Contravariant Writer where
  contramap f (Writer w) = Writer (w . f)

instance ProductContravariant Writer where
  point = Writer (const mempty)
  w ***< w' = writer (uncurry (++) . (runWriter w *** runWriter w'))

data PackMap a b = PackMap ((String -> String) -> a -> b)

runPackMap :: PackMap a b -> (String -> String) -> a -> b
runPackMap (PackMap p) = p

instance Profunctor PackMap where
  dimap f g (PackMap p) = PackMap (\s -> g . p s . f)

instance ProductProfunctor PackMap where
  empty = PackMap (\_ -> id)
  (PackMap p) ***! (PackMap p') = PackMap ((***) <$> p <*> p')

runWriter :: Writer t -> t -> [String]
runWriter (Writer w) x = w x

writer :: (t -> [String]) -> Writer t
writer = Writer

packMapWire :: PackMap (Wire a) (Wire a)
packMapWire = PackMap (\f -> Wire . f . unWire)

writerWire :: Writer (Wire a)
writerWire = Writer (return . unWire)

-- TODO: this needs a better name
data Colspec' a b = Colspec' (Writer a) (PackMap a b)

instance Profunctor Colspec' where
  dimap f g (Colspec' w p) = Colspec' (contramap f w) (dimap f g p)

instance ProductProfunctor Colspec' where
  empty = Colspec' point empty
  (Colspec' w p) ***! (Colspec' w' p') =
    Colspec' (w ***< w') (p ***! p')

runWriterOfColspec' :: Colspec' a b -> a -> [String]
runWriterOfColspec' (Colspec' f _) = runWriter f

runPackMapOfColspec' :: Colspec' a b -> (String -> String) -> a -> b
runPackMapOfColspec' (Colspec' _ p) = runPackMap p

-- This seems to be basically a Colspec' with the column names already applied
data Colspec a = Colspec [String] ((String -> String) -> a)

instance Functor Colspec where
  fmap f (Colspec s m) = Colspec s (f . m)

instance Applicative Colspec where
  pure = Colspec mempty . pure
  Colspec s mf <*> Colspec s' m = Colspec (s ++ s') (mf <*> m)

runWriterOfColspec :: Colspec a -> [String]
runWriterOfColspec (Colspec s _) = s

runPackMapOfColspec :: Colspec a -> (String -> String) -> a
runPackMapOfColspec (Colspec _ m) f = m f

colspec :: [String] -> ((String -> String) -> a) -> Colspec a
colspec s m = Colspec s m

col :: String -> Colspec (Wire a)
col s = colspec [s] (\f -> Wire (f s))
