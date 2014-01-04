{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Karamaan.Opaleye.ArrowLambda where

import Data.Profunctor (Profunctor, dimap)
import Control.Arrow (Arrow, arr, (<<<))
import Control.Applicative (Applicative, (<*>), pure)
import Data.Profunctor.Product.Default (Default, def)

newtype ArrowLambda arr a b c = ArrowLambda (arr a b -> c)

instance Arrow arr => Profunctor (ArrowLambda arr a) where
  dimap f g (ArrowLambda h) = ArrowLambda (dimap (arr f <<<) g h)

instance Functor (ArrowLambda arr a b) where
  fmap f (ArrowLambda g) = ArrowLambda (fmap f g)

instance Arrow arr => Applicative (ArrowLambda arr a b) where
  pure = ArrowLambda . pure
  ArrowLambda f <*> ArrowLambda x = ArrowLambda (f <*> x)

instance Default (ArrowLambda arr a) b (arr a b) where
  def = ArrowLambda id
