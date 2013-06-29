module Karamaan.Opaleye.Colspec where

import Karamaan.Opaleye.Pack (unflatten1, flatten1,
                              unflatten2, flatten2,
                              unflatten3, flatten3,
                              unflatten4, flatten4,
                              unflatten5, flatten5,
                              unflatten6, flatten6,
                              unflatten7, flatten7,
                              unflatten8, flatten8)
import Karamaan.Opaleye.Wire (Wire(Wire), unWire)
import Karamaan.Opaleye.Aggregate (Writer)
-- FIXME: don't want to import everything, but we're importing a lot
-- and I can't be bothered to type it all
import Karamaan.Opaleye.Aggregators hiding (chain, convert)

data Colspec a = Colspec a (Writer a)

col :: String -> Colspec (Wire a)
col s = Colspec (Wire s) (return . unWire)

colspecApp :: (b -> a) -> (a -> b) -> Colspec a -> Colspec b
colspecApp f g (Colspec a w) = Colspec (g a) (w . f)

colsT1 :: T1 (Colspec a1) -> Colspec (T1 a1)
colsT1 = id

-- TODO: dup with *:
colsT2 :: T2 (Colspec a1) (Colspec a2) -> Colspec (T2 a1 a2)
colsT2 (Colspec a1 w1, Colspec a2 w2)
  = Colspec (a1, a2) w'
  where w' (x1, x2) = w1 x1 ++ w2 x2

chain :: (t -> Colspec b) -> (Colspec a, t) -> Colspec (a, b)
chain rest (a, as) = colsT2 (a, rest as)

colsT3 :: T3 (Colspec a1) (Colspec a2) (Colspec a3) -> Colspec (T3 a1 a2 a3)
colsT3 = chain colsT2

colsT4 :: T4 (Colspec a1) (Colspec a2) (Colspec a3) (Colspec a4)
          -> Colspec (T4 a1 a2 a3 a4)
colsT4 = chain colsT3

colsT5 :: T5 (Colspec a1) (Colspec a2) (Colspec a3) (Colspec a4) (Colspec a5)
          -> Colspec (T5 a1 a2 a3 a4 a5)
colsT5 = chain colsT4

colsT6 :: T6 (Colspec a1) (Colspec a2) (Colspec a3) (Colspec a4) (Colspec a5)
          (Colspec a6)
          -> Colspec (T6 a1 a2 a3 a4 a5 a6)
colsT6 = chain colsT5

colsT7 :: T7 (Colspec a1) (Colspec a2) (Colspec a3) (Colspec a4) (Colspec a5)
          (Colspec a6) (Colspec a7)
          -> Colspec (T7 a1 a2 a3 a4 a5 a6 a7)
colsT7 = chain colsT6

colsT8 :: T8 (Colspec a1) (Colspec a2) (Colspec a3) (Colspec a4) (Colspec a5)
          (Colspec a6) (Colspec a7) (Colspec a8)
          -> Colspec (T8 a1 a2 a3 a4 a5 a6 a7 a8)
colsT8 = chain colsT7

convert :: (b -> a1) -> (a -> b1) -> (a1 -> b) -> (b1 -> Colspec a1)
          -> a -> Colspec b
convert u u' f c = colspecApp u f . c . u'

cols1 :: Colspec a -> Colspec a
cols1 = convert unflatten1 unflatten1 flatten1 colsT1

cols2 :: (Colspec a, Colspec b) -> Colspec (a, b)
cols2 = convert unflatten2 unflatten2 flatten2 colsT2

cols3 :: (Colspec a, Colspec b, Colspec a3) -> Colspec (a, b, a3)
cols3 = convert unflatten3 unflatten3 flatten3 colsT3

cols4 :: (Colspec a, Colspec b, Colspec a3, Colspec a4)
         -> Colspec (a, b, a3, a4)
cols4 = convert unflatten4 unflatten4 flatten4 colsT4

cols5 :: (Colspec a, Colspec b, Colspec a3, Colspec a4, Colspec a5)
         -> Colspec (a, b, a3, a4, a5)
cols5 = convert unflatten5 unflatten5 flatten5 colsT5

cols6 :: (Colspec a, Colspec b, Colspec a3, Colspec a4, Colspec a5, Colspec a6)
         -> Colspec (a, b, a3, a4, a5, a6)
cols6 = convert unflatten6 unflatten6 flatten6 colsT6

cols7 :: (Colspec a, Colspec b, Colspec a3, Colspec a4, Colspec a5, Colspec a6,
          Colspec a7)
         -> Colspec (a, b, a3, a4, a5, a6, a7)
cols7 = convert unflatten7 unflatten7 flatten7 colsT7

cols8 :: (Colspec a, Colspec b, Colspec a3, Colspec a4, Colspec a5, Colspec a6,
          Colspec a7, Colspec a8)
         -> Colspec (a, b, a3, a4, a5, a6, a7, a8)
cols8 = convert unflatten8 unflatten8 flatten8 colsT8
