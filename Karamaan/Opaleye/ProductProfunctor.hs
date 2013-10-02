module Karamaan.Opaleye.ProductProfunctor where

import Data.Profunctor (Profunctor)
import Data.Functor.Contravariant (Contravariant)
-- vv TODO: don't want to have to import all those explicitly.  What to do?
import Karamaan.Opaleye.Pack
-- vv and these
import Karamaan.Opaleye.Tuples

class Profunctor p => ProductProfunctor p where
  empty :: p () ()
  (***!) :: p a b -> p a' b' -> p (a, a') (b, b')

class Contravariant f => ProductContravariant f where
  point :: f ()
  (***<) :: f a -> f b -> f (a, b)

pT1 :: ProductProfunctor p => T1 (p a1 b1) -> p (T1 a1) (T1 b1)
pT1 = id

pT2 :: ProductProfunctor p => T2 (p a1 b1) (p a2 b2) -> p (T2 a1 a2) (T2 b1 b2)
pT2 = uncurry (***!)

chain :: ProductProfunctor p => (t -> p a2 b2) -> (p a1 b1, t)
      -> p (a1, a2) (b1, b2)
chain rest (a, as) = pT2 (a, rest as)

pT3 :: ProductProfunctor p => T3 (p a1 b1) (p a2 b2) (p a3 b3)
       -> p (T3 a1 a2 a3) (T3 b1 b2 b3)
pT3 = chain pT2

pT4 :: ProductProfunctor p => T4 (p a1 b1) (p a2 b2) (p a3 b3) (p a4 b4)
       -> p (T4 a1 a2 a3 a4) (T4 b1 b2 b3 b4)
pT4 = chain pT3

pT5 :: ProductProfunctor p => T5 (p a1 b1) (p a2 b2) (p a3 b3) (p a4 b4)
                                 (p a5 b5)
       -> p (T5 a1 a2 a3 a4 a5) (T5 b1 b2 b3 b4 b5)
pT5 = chain pT4

pT6 :: ProductProfunctor p => T6 (p a1 b1) (p a2 b2) (p a3 b3) (p a4 b4)
                                 (p a5 b5) (p a6 b6)
       -> p (T6 a1 a2 a3 a4 a5 a6) (T6 b1 b2 b3 b4 b5 b6)
pT6 = chain pT5

pT7 :: ProductProfunctor p => T7 (p a1 b1) (p a2 b2) (p a3 b3) (p a4 b4)
                                 (p a5 b5) (p a6 b6) (p a7 b7)
       -> p (T7 a1 a2 a3 a4 a5 a6 a7) (T7 b1 b2 b3 b4 b5 b6 b7)
pT7 = chain pT6

pT8 :: ProductProfunctor p => T8 (p a1 b1) (p a2 b2) (p a3 b3) (p a4 b4)
                                 (p a5 b5) (p a6 b6) (p a7 b7) (p a8 b8)
       -> p (T8 a1 a2 a3 a4 a5 a6 a7 a8) (T8 b1 b2 b3 b4 b5 b6 b7 b8)
pT8 = chain pT7

pT9 :: ProductProfunctor p => T9 (p a1 b1) (p a2 b2) (p a3 b3) (p a4 b4)
                                 (p a5 b5) (p a6 b6) (p a7 b7) (p a8 b8)
                                 (p a9 b9)
       -> p (T9 a1 a2 a3 a4 a5 a6 a7 a8 a9)
            (T9 b1 b2 b3 b4 b5 b6 b7 b8 b9)
pT9 = chain pT8

pT10 :: ProductProfunctor p => T10 (p a1 b1) (p a2 b2) (p a3 b3) (p a4 b4)
                                   (p a5 b5) (p a6 b6) (p a7 b7) (p a8 b8)
                                   (p a9 b9) (p a10 b10)
       -> p (T10 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
            (T10 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10)
pT10 = chain pT9

pT11 :: ProductProfunctor p => T11 (p a1 b1) (p a2 b2) (p a3 b3) (p a4 b4)
                                   (p a5 b5) (p a6 b6) (p a7 b7) (p a8 b8)
                                   (p a9 b9) (p a10 b10) (p a11 b11)
       -> p (T11 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11)
            (T11 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11)
pT11 = chain pT10

p0 :: ProductProfunctor p => () -> p () ()
p0 = const empty

p1 :: ProductProfunctor p => p a1 b1 -> p a1 b1
p1 = id

p2 :: ProductProfunctor p => p a1 b1 -> p a2 b2 -> p (a1, a2) (b1, b2)
p2 = (***!)
