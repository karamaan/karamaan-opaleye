{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
    ScopedTypeVariables #-}

module Karamaan.Opaleye.ArrowLambda
       {-# DEPRECATED "This is for exploratory purposes only" #-}
       where

import Data.Profunctor (Profunctor, dimap)
import Control.Arrow (Arrow, arr, (<<<))
import Control.Applicative (Applicative, (<*>), pure)
import Data.Profunctor.Product (ProductProfunctor, empty, (***!),
                                defaultEmpty, defaultProfunctorProduct)
import Data.Profunctor.Product.Default (Default, def)
import Karamaan.Opaleye.Operators2 ((.==.), (./=.), (.||.))
import Karamaan.Opaleye.QueryArr (QueryArr)
import Karamaan.Opaleye.Wire (Wire)
import qualified Control.Category as Cat

import Data.Profunctor.Product (p4)

newtype ArrowLambda arr a b c = ArrowLambda (arr a b -> c)

runArrowLambda :: Cat.Category arr => ArrowLambda arr b b c -> (c -> r) -> r
runArrowLambda (ArrowLambda g) f = (f . g) Cat.id

runArrowLambdaQ :: ArrowLambda QueryArr b b c -> (c -> QueryArr b q)
                   -> QueryArr b q
runArrowLambdaQ = runArrowLambda

runArrowLambdaP :: Proxy c
                   -> ArrowLambda QueryArr b b c
                   -> (c -> QueryArr b q)
                   -> QueryArr b q
runArrowLambdaP _ = runArrowLambdaQ

data Proxy a = Proxy

runArrowLambdaQ' :: Cat.Category arr => ArrowLambda arr a b c -> arr a b -> c
runArrowLambdaQ' (ArrowLambda g) = g

instance Arrow arr => Profunctor (ArrowLambda arr a) where
  dimap f g (ArrowLambda h) = ArrowLambda (dimap (arr f <<<) g h)

instance Functor (ArrowLambda arr a b) where
  fmap f (ArrowLambda g) = ArrowLambda (fmap f g)

instance Arrow arr => Applicative (ArrowLambda arr a b) where
  pure = ArrowLambda . pure
  ArrowLambda f <*> ArrowLambda x = ArrowLambda (f <*> x)

instance Arrow arr => ProductProfunctor (ArrowLambda arr a) where
  empty = defaultEmpty
  (***!) = defaultProfunctorProduct

var :: ArrowLambda arr a b (arr a b)
var = ArrowLambda id

instance Default (ArrowLambda arr a) b (arr a b) where
  def = var

{-

So basically the problem is that the lambda expression is too
polymorphic and the type class solver doesn't know that if it is
looking for an instance like

    Default (ArrowLambda arr a) b (arr a c)

then 'b' must equal 'c'.

It's possible we could use functional dependencies for that, but then
that would take us out of the nice automatic tupling arrangement that
Default gives us, and we would have to write it all by hand again, I
think.

There are two interim solutions:

  * Use a Proxy.  Unfortunately this makes you write out a messy type and
    requires ScopedTypeVariables.
  * Use your adaptor and var.  This is less typing, but still annoying.

I think the latter is neater.

-}

example :: forall b c. QueryArr (Wire b, Wire b, Wire c, Wire c) (Wire Bool)
example = runArrowLambdaP
              (Proxy :: Proxy (QueryArr a (Wire b), QueryArr a (Wire b),
                               QueryArr a (Wire c), QueryArr a (Wire c)))
              def
              (\(w, x, y, z) -> (w .==. x) .||. (y ./=. z))

example' :: forall b c. QueryArr (Wire b, Wire b, Wire c, Wire c) (Wire Bool)
example' = runArrowLambdaQ (p4 (var, var, var, var))
              (\(w, x, y, z) -> (w .==. x) .||. (y ./=. z))
