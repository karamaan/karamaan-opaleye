module Karamaan.Opaleye.Unpackspec where

import Karamaan.Opaleye.Colspec (Writer)
import Karamaan.Opaleye.Tuples
import Karamaan.Opaleye.Pack hiding (unpack)

newtype Unpackspec a = Unpackspec (Writer a)

unpackspecApp :: (b -> a) -> Unpackspec a -> Unpackspec b
unpackspecApp f (Unpackspec u) = Unpackspec (u . f)

convert :: (b -> a1) -> (a -> b1) -> (b1 -> Unpackspec a1) -> a -> Unpackspec b
convert u u' c = unpackspecApp u . c . u'

(^:) :: Unpackspec a -> Unpackspec b -> Unpackspec (a, b)
(Unpackspec f1) ^: (Unpackspec f2) = Unpackspec (\(x1, x2) -> f1 x1 ++ f2 x2)

chain :: (t -> Unpackspec a2) -> (Unpackspec a1, t) -> Unpackspec (a1, a2)
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

unpackT :: Unpackspec a -> Unpackspec a
unpackT = unpackT1

unpack2 :: (Unpackspec a1, Unpackspec a2) -> Unpackspec (a1, a2)
unpack2 = convert unflatten2 unflatten2 unpackT2

unpack3 :: (Unpackspec a1, Unpackspec a2, Unpackspec a3)
           -> Unpackspec (a1, a2, a3)
unpack3 = convert unflatten3 unflatten3 unpackT3

unpack4 :: (Unpackspec a1, Unpackspec a2, Unpackspec a3, Unpackspec a4)
           -> Unpackspec (a1, a2, a3, a4)
unpack4 = convert unflatten4 unflatten4 unpackT4

unpack5 :: (Unpackspec a1, Unpackspec a2, Unpackspec a3, Unpackspec a4,
            Unpackspec a5)
           -> Unpackspec (a1, a2, a3, a4, a5)
unpack5 = convert unflatten5 unflatten5 unpackT5

unpack6 :: (Unpackspec a1, Unpackspec a2, Unpackspec a3, Unpackspec a4,
            Unpackspec a5, Unpackspec a6)
           -> Unpackspec (a1, a2, a3, a4, a5, a6)
unpack6 = convert unflatten6 unflatten6 unpackT6

unpack7 :: (Unpackspec a1, Unpackspec a2, Unpackspec a3, Unpackspec a4,
            Unpackspec a5, Unpackspec a6, Unpackspec a7)
           -> Unpackspec (a1, a2, a3, a4, a5, a6, a7)
unpack7 = convert unflatten7 unflatten7 unpackT7

unpack8 :: (Unpackspec a1, Unpackspec a2, Unpackspec a3, Unpackspec a4,
            Unpackspec a5, Unpackspec a6, Unpackspec a7, Unpackspec a8)
           -> Unpackspec (a1, a2, a3, a4, a5, a6, a7, a8)
unpack8 = convert unflatten8 unflatten8 unpackT8
