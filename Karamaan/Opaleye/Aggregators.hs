module Karamaan.Opaleye.Aggregators where

import Karamaan.Opaleye.Aggregate (Aggregator(Aggregator), (*:))
import Karamaan.Opaleye.Tuples (ap8, pure8, toList8)
import Karamaan.Opaleye.Pack (unflatten1, flatten1,
                              unflatten2, flatten2,
                              unflatten3, flatten3,
                              unflatten4, flatten4,
                              unflatten5, flatten5,
                              unflatten6, flatten6,
                              unflatten7, flatten7,
                              unflatten8, flatten8)

-- These really belong in Karamaan.Opaleye.Aggregate but the
-- implementations are somewhat verbose so I wanted to sequester them

type T1 a = a
type T2 a b = (a, T1 b)
type T3 a b c = (a, T2 b c)
type T4 a b c d = (a, T3 b c d)
type T5 a b c d e = (a, T4 b c d e)
type T6 a b c d e f = (a, T5 b c d e f)
type T7 a b c d e f g = (a, T6 b c d e f g)
type T8 a b c d e f g h = (a, T7 b c d e f g h)

aggApp :: (a -> b) -> (b -> a) -> Aggregator a -> Aggregator b
aggApp f f' (Aggregator s w p) = Aggregator s (w . f') p'
  where p' g = f . p g . f'

chain :: (t -> Aggregator a') -> (Aggregator a, t) -> Aggregator (a, a')
chain agg (a, as) = a *: agg as

-- Need two unflattens (u, u') because of insufficient polymorphism, I guess
cobber :: (a' -> b) -> (b -> a') -> (a -> b') -> (b' -> Aggregator a') -> a
          -> Aggregator b
cobber f u u' agg = aggApp f u . agg . u'

agg1T :: Aggregator a1 -> Aggregator (T1 a1)
agg1T = id

agg2T :: T2 (Aggregator a1) (Aggregator a2)
        -> Aggregator (T2 a1 a2)
agg2T = chain agg1T

agg3T:: T3 (Aggregator a1) (Aggregator a2) (Aggregator a3)
        -> Aggregator (T3 a1 a2 a3)
agg3T = chain agg2T

agg4T:: T4 (Aggregator a1) (Aggregator a2) (Aggregator a3) (Aggregator a4)
        -> Aggregator (T4 a1 a2 a3 a4)
agg4T = chain agg3T

agg5T:: T5 (Aggregator a1) (Aggregator a2) (Aggregator a3) (Aggregator a4)
           (Aggregator a5)
        -> Aggregator (T5 a1 a2 a3 a4 a5)
agg5T = chain agg4T

agg6T:: T6 (Aggregator a1) (Aggregator a2) (Aggregator a3) (Aggregator a4)
           (Aggregator a5) (Aggregator a6)
        -> Aggregator (T6 a1 a2 a3 a4 a5 a6)
agg6T = chain agg5T

agg7T:: T7 (Aggregator a1) (Aggregator a2) (Aggregator a3) (Aggregator a4)
           (Aggregator a5) (Aggregator a6) (Aggregator a7)
        -> Aggregator (T7 a1 a2 a3 a4 a5 a6 a7)
agg7T = chain agg6T

agg8T:: T8 (Aggregator a1) (Aggregator a2) (Aggregator a3) (Aggregator a4)
           (Aggregator a5) (Aggregator a6) (Aggregator a7) (Aggregator a8)
        -> Aggregator (T8 a1 a2 a3 a4 a5 a6 a7 a8)
agg8T = chain agg7T

agg1 :: Aggregator a1 -> Aggregator a1
agg1 = cobber flatten1 unflatten1 unflatten1 agg1T

agg2 :: (Aggregator a1, Aggregator a2)
        -> Aggregator (a1, a2)
agg2 = cobber flatten2 unflatten2 unflatten2 agg2T

agg3 :: (Aggregator a1, Aggregator a2, Aggregator a3)
        -> Aggregator (a1, a2, a3)
agg3 = cobber flatten3 unflatten3 unflatten3 agg3T

agg4 :: (Aggregator a1, Aggregator a2, Aggregator a3, Aggregator a4)
        -> Aggregator (a1, a2, a3, a4)
agg4 = cobber flatten4 unflatten4 unflatten4 agg4T

agg5 :: (Aggregator a1, Aggregator a2, Aggregator a3, Aggregator a4,
         Aggregator a5)
        -> Aggregator (a1, a2, a3, a4, a5)
agg5 = cobber flatten5 unflatten5 unflatten5 agg5T

agg6 :: (Aggregator a1, Aggregator a2, Aggregator a3, Aggregator a4,
         Aggregator a5, Aggregator a6)
        -> Aggregator (a1, a2, a3, a4, a5, a6)
agg6 = cobber flatten6 unflatten6 unflatten6 agg6T

agg7 :: (Aggregator a1, Aggregator a2, Aggregator a3, Aggregator a4,
         Aggregator a5, Aggregator a6, Aggregator a7)
        -> Aggregator (a1, a2, a3, a4, a5, a6, a7)
agg7 = cobber flatten7 unflatten7 unflatten7 agg7T

agg8 :: (Aggregator a1, Aggregator a2, Aggregator a3, Aggregator a4,
         Aggregator a5, Aggregator a6, Aggregator a7, Aggregator a8)
        -> Aggregator (a1, a2, a3, a4, a5, a6, a7, a8)
agg8 = cobber flatten8 unflatten8 unflatten8 agg8T
