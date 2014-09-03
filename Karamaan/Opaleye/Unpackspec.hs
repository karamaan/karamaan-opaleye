{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Karamaan.Opaleye.Unpackspec where

import Karamaan.Opaleye.QueryColspec (Writer, writerWire)
import qualified Karamaan.Opaleye.QueryColspec as Q
import Data.Profunctor.Product.Tuples
import Data.Profunctor.Product.Flatten
import Data.Profunctor.Product ((***<), ProductContravariant, point,
                                PPOfContravariant(PPOfContravariant),
                                defaultPoint, defaultContravariantProduct)
import Data.Functor.Contravariant (Contravariant, contramap)
import Control.Arrow (second)
import Karamaan.Plankton ((.:))
import Data.Profunctor.Product.Default (Default, def)
import Karamaan.Opaleye.Wire (Wire)
import Data.Monoid (Monoid, (<>), mappend, mempty)

newtype Unpackspec a = Unpackspec (Writer a)

instance Contravariant Unpackspec where
  contramap f (Unpackspec w) = Unpackspec (contramap f w)

instance Monoid (Unpackspec a) where
  mempty = Unpackspec mempty
  Unpackspec w `mappend` Unpackspec w' = Unpackspec (w <> w')

instance ProductContravariant Unpackspec where
  point = defaultPoint
  (***<) = defaultContravariantProduct

instance Default (PPOfContravariant Unpackspec) (Wire a) (Wire a) where
  def = PPOfContravariant unpackspecWire

unpackspecWire :: Unpackspec (Wire a)
unpackspecWire = Unpackspec writerWire

runUnpackspec :: Unpackspec a -> a -> [String]
runUnpackspec (Unpackspec writer) = Q.runWriter writer

convert :: ProductContravariant f => (b -> a1) -> (a -> b1) -> (b1 -> f a1) -> a -> f b
convert u u' c = contramap u . c . u'

chain :: ProductContravariant f => (t -> f a2) -> (f a1, t) -> f (a1, a2)
chain = pcT2 .: second

pcT1 :: ProductContravariant f => T1 (f a) -> f (T1 a)
pcT1 = id

pcT2 :: ProductContravariant f => T2 (f a1) (f a2) -> f (T2 a1 a2)
pcT2 = uncurry (***<)

pcT3 :: ProductContravariant f => T3 (f a1) (f a2) (f a3)
            -> f (T3 a1 a2 a3)
pcT3 = chain pcT2

pcT4 :: ProductContravariant f => T4 (f a1) (f a2) (f a3) (f a4)
            -> f (T4 a1 a2 a3 a4)
pcT4 = chain pcT3

pcT5 :: ProductContravariant f => T5 (f a1) (f a2) (f a3) (f a4)
               (f a5)
            -> f (T5 a1 a2 a3 a4 a5)
pcT5 = chain pcT4

pcT6 :: ProductContravariant f => T6 (f a1) (f a2) (f a3) (f a4)
               (f a5) (f a6)
            -> f (T6 a1 a2 a3 a4 a5 a6)
pcT6 = chain pcT5

pcT7 :: ProductContravariant f => T7 (f a1) (f a2) (f a3) (f a4)
               (f a5) (f a6) (f a7)
            -> f (T7 a1 a2 a3 a4 a5 a6 a7)
pcT7 = chain pcT6

pcT8 :: ProductContravariant f => T8 (f a1) (f a2) (f a3) (f a4)
               (f a5) (f a6) (f a7) (f a8)
            -> f (T8 a1 a2 a3 a4 a5 a6 a7 a8)
pcT8 = chain pcT7

pcT :: ProductContravariant f => f a -> f a
pcT = pcT1

pc2 :: ProductContravariant f => (f a1, f a2) -> f (a1, a2)
pc2 = convert unflatten2 unflatten2 pcT2

pc3 :: ProductContravariant f => (f a1, f a2, f a3)
           -> f (a1, a2, a3)
pc3 = convert unflatten3 unflatten3 pcT3

pc4 :: ProductContravariant f => (f a1, f a2, f a3, f a4)
           -> f (a1, a2, a3, a4)
pc4 = convert unflatten4 unflatten4 pcT4

pc5 :: ProductContravariant f => (f a1, f a2, f a3, f a4,
            f a5)
           -> f (a1, a2, a3, a4, a5)
pc5 = convert unflatten5 unflatten5 pcT5

pc6 :: ProductContravariant f => (f a1, f a2, f a3, f a4,
            f a5, f a6)
           -> f (a1, a2, a3, a4, a5, a6)
pc6 = convert unflatten6 unflatten6 pcT6

pc7 :: ProductContravariant f => (f a1, f a2, f a3, f a4,
            f a5, f a6, f a7)
           -> f (a1, a2, a3, a4, a5, a6, a7)
pc7 = convert unflatten7 unflatten7 pcT7

pc8 :: ProductContravariant f => (f a1, f a2, f a3, f a4,
            f a5, f a6, f a7, f a8)
           -> f (a1, a2, a3, a4, a5, a6, a7, a8)
pc8 = convert unflatten8 unflatten8 pcT8
