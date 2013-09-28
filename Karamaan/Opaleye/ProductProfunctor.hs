module Karamaan.Opaleye.ProductProfunctor where

import Data.Profunctor (Profunctor)

class Profunctor p => ProductProfunctor p where
  empty :: p () ()
  (***!) :: p a b -> p a' b' -> p (a, a') (b, b')
