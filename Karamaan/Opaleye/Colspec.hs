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
-- FIXME: don't want to import everything, but we're importing a lot
-- and I can't be bothered to type it all
import Karamaan.Opaleye.Tuples
import Control.Arrow ((***))

newtype Writer a = Writer (a -> [String])
type PackMap a = (String -> String) -> a -> a

modifyWriter :: Writer b -> (a -> b) -> Writer a
modifyWriter w f = writer (runWriter w . f)

runWriter :: Writer t -> t -> [String]
runWriter (Writer w) x = w x

writer :: (t -> [String]) -> Writer t
writer = Writer

-- FIXME: These are ridiculous names
(+++) :: Writer a -> Writer b -> Writer (a, b)
w +++ w' = writer (uncurry (++) . (runWriter w *** runWriter w'))

(++++) :: PackMap a -> PackMap b -> PackMap (a, b)
f ++++ g = \ss -> f ss *** g ss

-- TODO: when I added the PackMap argument to Colspec I only had to update
-- col, colspecApp and colsT2.  I didn't have to add an argument to colspecApp,
-- interesting.  What are the implications for this way of doing "generics" on
-- tuples?
data Colspec a = Colspec a (Writer a) (PackMap a)

col :: String -> Colspec (Wire a)
col s = Colspec (Wire s) (writer (return . unWire)) (\f -> Wire . f . unWire)

colspecApp :: (b -> a) -> (a -> b) -> Colspec a -> Colspec b
colspecApp f g (Colspec a w p) = Colspec (g a) (modifyWriter w f) (\ss -> g . p ss . f)

colsT1 :: T1 (Colspec a1) -> Colspec (T1 a1)
colsT1 = id

-- TODO: dup with *:
colsT2 :: T2 (Colspec a1) (Colspec a2) -> Colspec (T2 a1 a2)
colsT2 (Colspec a1 w1 p1, Colspec a2 w2 p2)
  = Colspec (a1, a2) w' p'
  where w' = w1 +++ w2
        p' = p1 ++++ p2

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
