module Karamaan.Opaleye.QueryArr where

import Karamaan.Opaleye.Colspec (Writer)
import Control.Arrow ((***))
import Data.Function (on)
import Karamaan.Opaleye.Tuples

newtype Unpackspec a = Unpackspec (Writer a)

unUnpackspec :: Unpackspec a -> Writer a
unUnpackspec (Unpackspec f) = f

(^:) :: Unpackspec a -> Unpackspec b -> Unpackspec (a, b)
(Unpackspec f1) ^: (Unpackspec f2) = Unpackspec (\(x1, x2) -> f1 x1 ++ f2 x2)

chain unpack (a, as) = unpackT2 (a, unpack as)

unpackT1 :: T1 (Unpackspec a) -> Unpackspec (T1 a)
unpackT1 = id

unpackT2 :: T2 (Unpackspec a1) (Unpackspec a2) -> Unpackspec (T2 a1 a2)
unpackT2 = uncurry (^:)

unpackT3 :: T3 (Unpackspec a1) (Unpackspec a2) (Unpackspec a3)
            -> Unpackspec (T3 a1 a2 a3)
unpackT3 = chain unpackT2

unpackT4 :: T4 (Unpackspec a1) (Unpackspec a2) (Unpackspec a3) (Unpackspec a4)
            -> Unpackspec (T4 a1 a2 a3 a4)
unpackT4 = chain unpackT3

unpackT5 :: T5 (Unpackspec a1) (Unpackspec a2) (Unpackspec a3) (Unpackspec a4)
               (Unpackspec a5)
            -> Unpackspec (T5 a1 a2 a3 a4 a5)
unpackT5 = chain unpackT4

unpackT6 :: T6 (Unpackspec a1) (Unpackspec a2) (Unpackspec a3) (Unpackspec a4)
               (Unpackspec a5) (Unpackspec a6)
            -> Unpackspec (T6 a1 a2 a3 a4 a5 a6)
unpackT6 = chain unpackT5

unpackT7 :: T7 (Unpackspec a1) (Unpackspec a2) (Unpackspec a3) (Unpackspec a4)
               (Unpackspec a5) (Unpackspec a6) (Unpackspec a7)
            -> Unpackspec (T7 a1 a2 a3 a4 a5 a6 a7)
unpackT7 = chain unpackT6

unpackT8 :: T8 (Unpackspec a1) (Unpackspec a2) (Unpackspec a3) (Unpackspec a4)
               (Unpackspec a5) (Unpackspec a6) (Unpackspec a7) (Unpackspec a8)
            -> Unpackspec (T8 a1 a2 a3 a4 a5 a6 a7 a8)
unpackT8 = chain unpackT7
