module Karamaan.Opaleye.Aggregators where

import Karamaan.Opaleye.Aggregate (Aggregator(Aggregator))

-- These really belong in Karamaan.Opaleye.Aggregate but the
-- implementations are such verbose boilerplate that I wanted to
-- sequester them until I work out how to deal with that boilerplate

-- There's a lot of boilerplate here!
-- Not sure what to do to get rid of that
agg8 :: (Aggregator a1, Aggregator a2, Aggregator a3, Aggregator a4,
         Aggregator a5, Aggregator a6, Aggregator a7, Aggregator a8)
        -> Aggregator (a1, a2, a3, a4, a5, a6, a7, a8)
agg8 (Aggregator s1 w1 p1, Aggregator s2 w2 p2,
      Aggregator s3 w3 p3, Aggregator s4 w4 p4,
      Aggregator s5 w5 p5, Aggregator s6 w6 p6,
      Aggregator s7 w7 p7, Aggregator s8 w8 p8)
  = Aggregator s' w' p'
  where s' = concat [s1, s2, s3, s4, s5, s6, s7, s8]
        w' (x1, x2, x3, x4, x5, x6, x7, x8)
          = concat [w1 x1, w2 x2, w3 x3, w4 x4,
                    w5 x5, w6 x6, w7 x7, w8 x8]
        p' f (x1, x2, x3, x4, x5, x6, x7, x8)
          = (p1 f x1, p2 f x2, p3 f x3, p4 f x4,
             p5 f x5, p6 f x6, p7 f x7, p8 f x8)

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
