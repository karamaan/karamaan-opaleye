module Karamaan.Opaleye.ProductProfunctor where

import Data.Profunctor (Profunctor)
import Data.Functor.Contravariant (Contravariant)

class Profunctor p => ProductProfunctor p where
  empty :: p () ()
  (***!) :: p a b -> p a' b' -> p (a, a') (b, b')

class Contravariant f => ProductContravariant f where
  point :: f ()
  (***<) :: f a -> f b -> f (a, b)

p0 :: ProductProfunctor p => () -> p () ()
p0 = const empty

p1 :: ProductProfunctor p => p a1 b1 -> p a1 b1
p1 = id

p2 :: ProductProfunctor p => p a1 b1 -> p a2 b2 -> p (a1, a2) (b1, b2)
p2 = (***!)
