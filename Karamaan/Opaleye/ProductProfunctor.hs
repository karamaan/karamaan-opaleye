module Karamaan.Opaleye.ProductProfunctor where

import Data.Profunctor (Profunctor)
import Data.Functor.Contravariant (Contravariant)

class Profunctor p => ProductProfunctor p where
  empty :: p () ()
  (***!) :: p a b -> p a' b' -> p (a, a') (b, b')

class Contravariant f => ProductContravariant f where
  (***<) :: f a -> f b -> f (a, b)
