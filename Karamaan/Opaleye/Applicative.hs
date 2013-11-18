module Karamaan.Opaleye.Applicative where

import Control.Applicative (Applicative, (<$>), (<*>), pure)
-- vv TODO: don't want to have to import all those explicitly.  What to do?
import Data.Profunctor.Product.Flatten
-- vv and these
import Data.Profunctor.Product.Tuples

liftA0 :: Applicative f => r -> f r
liftA0 = pure

liftA1 :: Applicative f => (a1 -> r) -> (f a1 -> f r)
liftA1 f x1 = f <$> x1

liftA2 :: Applicative f => (a1 -> a2 -> r)
          -> (f a1 -> f a2 -> f r)
liftA2 f x1 x2 = f <$> x1 <*> x2

liftA3 :: Applicative f => (a1 -> a2 -> a3  -> r)
          -> (f a1 -> f a2 -> f a3 -> f r)
liftA3 f x1 x2 x3 = f <$> x1 <*> x2 <*> x3

liftA4 :: Applicative f => (a1 -> a2 -> a3 -> a4 -> r)
          -> (f a1 -> f a2 -> f a3 -> f a4 -> f r)
liftA4 f x1 x2 x3 x4 = f <$> x1 <*> x2 <*> x3 <*> x4

liftA5 :: Applicative f => (a1 -> a2 -> a3 -> a4 -> a5 -> r)
          -> (f a1 -> f a2 -> f a3 -> f a4 -> f a5 -> f r)
liftA5 f x1 x2 x3 x4 x5 = f <$> x1 <*> x2 <*> x3 <*> x4 <*> x5

liftA6 :: Applicative f => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r)
          -> (f a1 -> f a2 -> f a3 -> f a4 -> f a5 -> f a6 -> f r)
liftA6 f x1 x2 x3 x4 x5 x6 = f <$> x1 <*> x2 <*> x3 <*> x4 <*> x5 <*> x6

liftA7 :: Applicative f => (a1 -> a2 -> a3 -> a4 -> a5 -> a6
                            -> a7 -> r)
          -> (f a1 -> f a2 -> f a3 -> f a4 -> f a5 -> f a6
              -> f a7 -> f r)
liftA7 f x1 x2 x3 x4 x5 x6 x7 = f <$> x1 <*> x2 <*> x3 <*> x4 <*> x5 <*> x6
                                  <*> x7

liftA8 :: Applicative f => (a1 -> a2 -> a3 -> a4 -> a5 -> a6
                            -> a7 -> a8 -> r)
       -> (f a1 -> f a2 -> f a3 -> f a4 -> f a5 -> f a6
           -> f a7 -> f a8 -> f r)
liftA8 f x1 x2 x3 x4 x5 x6 x7 x8 = f <$> x1 <*> x2 <*> x3 <*> x4 <*> x5 <*> x6
                                     <*> x7 <*> x8

liftA9 :: Applicative f => (a1 -> a2 -> a3 -> a4 -> a5 -> a6
                            -> a7 -> a8 -> a9 -> r)
       -> (f a1 -> f a2 -> f a3 -> f a4 -> f a5 -> f a6
           -> f a7 -> f a8 -> f a9 -> f r)
liftA9 f x1 x2 x3 x4 x5 x6 x7 x8 x9
  = f <$> x1 <*> x2 <*> x3 <*> x4 <*> x5 <*> x6
      <*> x7 <*> x8 <*> x9

liftA10 :: Applicative f => (a1 -> a2 -> a3 -> a4 -> a5 -> a6
                            -> a7 -> a8 -> a9 -> a10 -> r)
        -> (f a1 -> f a2 -> f a3 -> f a4 -> f a5 -> f a6
            -> f a7 -> f a8 -> f a9 -> f a10 -> f r)
liftA10 f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10
  = f <$> x1 <*> x2 <*> x3 <*> x4 <*> x5 <*> x6
      <*> x7 <*> x8 <*> x9 <*> x10

colspecApp :: Applicative f => (b -> a) -> (a -> b) -> f a -> f b
colspecApp _ = fmap

aT1 :: Applicative f => T1 (f a1) -> f (T1 a1)
aT1 = id

aT2 :: Applicative f => T2 (f a1) (f a2) -> f (T2 a1 a2)
aT2 (c, c') = (,) <$> c <*> c'

chain :: Applicative f => (t -> f b) -> (f a, t) -> f (a, b)
chain rest (a, as) = aT2 (a, rest as)

aT3 :: Applicative f => T3 (f a1) (f a2) (f a3) -> f (T3 a1 a2 a3)
aT3 = chain aT2

aT4 :: Applicative f => T4 (f a1) (f a2) (f a3) (f a4)
          -> f (T4 a1 a2 a3 a4)
aT4 = chain aT3

aT5 :: Applicative f => T5 (f a1) (f a2) (f a3) (f a4) (f a5)
          -> f (T5 a1 a2 a3 a4 a5)
aT5 = chain aT4

aT6 :: Applicative f => T6 (f a1) (f a2) (f a3) (f a4) (f a5)
          (f a6)
          -> f (T6 a1 a2 a3 a4 a5 a6)
aT6 = chain aT5

aT7 :: Applicative f => T7 (f a1) (f a2) (f a3) (f a4) (f a5)
          (f a6) (f a7)
          -> f (T7 a1 a2 a3 a4 a5 a6 a7)
aT7 = chain aT6

aT8 :: Applicative f => T8 (f a1) (f a2) (f a3) (f a4) (f a5)
          (f a6) (f a7) (f a8)
          -> f (T8 a1 a2 a3 a4 a5 a6 a7 a8)
aT8 = chain aT7

aT9 :: Applicative f => T9 (f a1) (f a2) (f a3) (f a4) (f a5)
          (f a6) (f a7) (f a8) (f a9)
          -> f (T9 a1 a2 a3 a4 a5 a6 a7 a8 a9)
aT9 = chain aT8

aT10 :: Applicative f => T10 (f a1) (f a2) (f a3) (f a4) (f a5)
          (f a6) (f a7) (f a8) (f a9) (f a10)
          -> f (T10 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
aT10 = chain aT9

convert :: Applicative f => (b -> a1) -> (a -> b1) -> (a1 -> b) -> (b1 -> f a1)
          -> a -> f b
convert u u' f c = colspecApp u f . c . u'

a1 :: Applicative f => f a -> f a
a1 = convert unflatten1 unflatten1 flatten1 aT1

a2 :: Applicative f => (f a, f b) -> f (a, b)
a2 = convert unflatten2 unflatten2 flatten2 aT2

a3 :: Applicative f => (f a, f b, f a3) -> f (a, b, a3)
a3 = convert unflatten3 unflatten3 flatten3 aT3

a4 :: Applicative f => (f a, f b, f a3, f a4)
         -> f (a, b, a3, a4)
a4 = convert unflatten4 unflatten4 flatten4 aT4

a5 :: Applicative f => (f a, f b, f a3, f a4, f a5)
         -> f (a, b, a3, a4, a5)
a5 = convert unflatten5 unflatten5 flatten5 aT5

a6 :: Applicative f => (f a, f b, f a3, f a4, f a5, f a6)
         -> f (a, b, a3, a4, a5, a6)
a6 = convert unflatten6 unflatten6 flatten6 aT6

a7 :: Applicative f => (f a, f b, f a3, f a4, f a5, f a6,
          f a7)
         -> f (a, b, a3, a4, a5, a6, a7)
a7 = convert unflatten7 unflatten7 flatten7 aT7

a8 :: Applicative f => (f a, f b, f a3, f a4, f a5, f a6,
          f a7, f a8)
         -> f (a, b, a3, a4, a5, a6, a7, a8)
a8 = convert unflatten8 unflatten8 flatten8 aT8

a9 :: Applicative f => (f a1, f a2, f a3, f a4, f a5,f a6,
          f a7, f a8, f a9)
         -> f (a1, a2, a3, a4, a5, a6, a7, a8, a9)
a9 = convert unflatten9 unflatten9 flatten9 aT9

a10 :: Applicative f => (f a1, f a2, f a3, f a4,f a5,f a6,
          f a7, f a8, f a9, f a10)
         -> f (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
a10 = convert unflatten10 unflatten10 flatten10 aT10
