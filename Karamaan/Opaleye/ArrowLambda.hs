{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module Karamaan.Opaleye.ArrowLambda where

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

newtype ArrowLambda arr a b c = ArrowLambda (arr a b -> c)

runArrowLambda :: Cat.Category arr => ArrowLambda arr b b c -> (c -> r) -> r
runArrowLambda (ArrowLambda g) f = (f . g) Cat.id

runArrowLambdaQ :: ArrowLambda QueryArr b b b -> (b -> r) -> r
runArrowLambdaQ = runArrowLambda

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

-- Would like to do this but it seems to get us into trouble with
-- incoherent instances
--instance Default (ArrowLambda arr a) b (arr a b) where
--  def = var

instance Default (ArrowLambda QueryArr a) (Wire b) (QueryArr a (Wire b)) where
  def = var

runArrowLambdaDef :: Default (ArrowLambda QueryArr c) c c
                     => (c -> r)
                     -> r
runArrowLambdaDef = runArrowLambdaQ def

example :: QueryArr (Wire b, Wire b, Wire c, Wire c) (Wire Bool)
example = runArrowLambda p $ \(w, x, y, z) -> (w .==. x) .||. (y ./=. z)

p :: ArrowLambda QueryArr a (Wire b1, Wire b2, Wire b3, Wire b4)
       (QueryArr a (Wire b1), QueryArr a (Wire b2), QueryArr a (Wire b3), QueryArr a (Wire b4))
p = def
