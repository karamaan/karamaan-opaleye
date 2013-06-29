module Karamaan.Opaleye.Aggregators where

import Karamaan.Opaleye.Aggregate (Aggregator(Aggregator), (*:))
import Karamaan.Opaleye.Tuples (ap8, pure8, toList8)
import Karamaan.Opaleye.Pack (unflatten3, flatten3,
                              unflatten4, flatten4
                             )

packMap (Aggregator _ _ p) = p
aggrops (Aggregator s _ _) = s
writer  (Aggregator _ w _) = w

-- These really belong in Karamaan.Opaleye.Aggregate but the
-- implementations are such verbose boilerplate that I wanted to
-- sequester them until I work out how to deal with that boilerplate

-- TODO: do it with tuple flattening

aggApp :: (a -> b) -> (b -> a) -> Aggregator a -> Aggregator b
aggApp f f' (Aggregator s w p) = Aggregator s (w . f') p'
  where p' g = f . p g . f'

jobber :: ((t1, t2) -> Aggregator a')
          -> (Aggregator a, t1, t2) -> Aggregator (a, a')
jobber jobber' aggs = a *: jobber' as
  where (a, as) = unflatten3 aggs

type T1 a = a
type T2 a b = (a, T1 b)
type T3 a b c = (a, T2 b c)
type T4 a b c d = (a, T3 b c d)
type T5 a b c d e = (a, T4 b c d e)
type T6 a b c d e f = (a, T5 b c d e f)
type T7 a b c d e f g = (a, T6 b c d e f g)
type T8 a b c d e f g h = (a, T7 b c d e f g h)

bojjer :: (t -> Aggregator a') -> (Aggregator a, t) -> Aggregator (a, a')
bojjer agg (a, as) = a *: agg as

agg2 :: (Aggregator a1, Aggregator a2)
        -> Aggregator (a1, a2)
agg2 = uncurry (*:)

agg2T :: T2 (Aggregator a1) (Aggregator a2)
        -> Aggregator (T2 a1 a2)
agg2T = agg2

agg3T:: T3 (Aggregator a1) (Aggregator a2) (Aggregator a3)
        -> Aggregator (T3 a1 a2 a3)
agg3T = bojjer agg2T

agg4T:: T4 (Aggregator a1) (Aggregator a2) (Aggregator a3) (Aggregator a4)
        -> Aggregator (T4 a1 a2 a3 a4)
agg4T = bojjer agg3T

agg5T:: T5 (Aggregator a1) (Aggregator a2) (Aggregator a3) (Aggregator a4)
           (Aggregator a5)
        -> Aggregator (T5 a1 a2 a3 a4 a5)
agg5T = bojjer agg4T

agg6T:: T6 (Aggregator a1) (Aggregator a2) (Aggregator a3) (Aggregator a4)
           (Aggregator a5) (Aggregator a6)
        -> Aggregator (T6 a1 a2 a3 a4 a5 a6)
agg6T = bojjer agg5T

agg7T:: T7 (Aggregator a1) (Aggregator a2) (Aggregator a3) (Aggregator a4)
           (Aggregator a5) (Aggregator a6) (Aggregator a7)
        -> Aggregator (T7 a1 a2 a3 a4 a5 a6 a7)
agg7T = bojjer agg6T

agg8T:: T8 (Aggregator a1) (Aggregator a2) (Aggregator a3) (Aggregator a4)
           (Aggregator a5) (Aggregator a6) (Aggregator a7) (Aggregator a8)
        -> Aggregator (T8 a1 a2 a3 a4 a5 a6 a7 a8)
agg8T = bojjer agg7T

agg3 :: (Aggregator a1, Aggregator a2, Aggregator a3)
        -> Aggregator (a1, a2, a3)
agg3 aggs = aggApp flatten3 unflatten3 bs
  where (a, as) = unflatten3 aggs
        bs = a *: agg2 as

-- There's a lot of boilerplate here!
-- Not sure what to do to get rid of that
agg8 :: (Aggregator a1, Aggregator a2, Aggregator a3, Aggregator a4,
         Aggregator a5, Aggregator a6, Aggregator a7, Aggregator a8)
        -> Aggregator (a1, a2, a3, a4, a5, a6, a7, a8)
agg8 aggs = Aggregator s' w' p'
  where s' = (concat . toList8) (aggropss `ap8` aggs)
        w' x
          = (concat . toList8) ((writers `ap8` aggs) `ap8` x)
        p' f x = ((packMaps `ap8` aggs) `ap8` (pure8 f)) `ap8` x
            -- yeah we need to repeat m because we don't have rank 2 types
        packMaps = (m, m, m, m, m, m, m, m)
        m = packMap
        aggropss = (s, s, s, s, s, s, s, s)
        s = aggrops
        writers = (w, w, w, w, w, w, w, w)
        w = writer

agg6 :: (Aggregator a1, Aggregator a2, Aggregator a3, Aggregator a4,
         Aggregator a5, Aggregator a6)
        -> Aggregator (a1, a2, a3, a4, a5, a6)
agg6 (Aggregator s1 w1 p1, Aggregator s2 w2 p2,
      Aggregator s3 w3 p3, Aggregator s4 w4 p4,
      Aggregator s5 w5 p5, Aggregator s6 w6 p6)
  = Aggregator s' w' p'
  where s' = concat [s1, s2, s3, s4, s5, s6]
        w' (x1, x2, x3, x4, x5, x6)
          = concat [w1 x1, w2 x2, w3 x3, w4 x4,
                    w5 x5, w6 x6]
        p' f (x1, x2, x3, x4, x5, x6)
          = (p1 f x1, p2 f x2, p3 f x3, p4 f x4,
             p5 f x5, p6 f x6)
