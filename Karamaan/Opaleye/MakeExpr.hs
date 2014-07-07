{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module Karamaan.Opaleye.MakeExpr where

import qualified Karamaan.Opaleye.ExprArr as E
import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP
import Data.Profunctor.Product ((***!))
import qualified Database.HaskellDB.Query as Q
import Karamaan.Opaleye.Wire (Wire)
import qualified Data.Profunctor.Product.Default as D
import qualified Control.Arrow as A
import Control.Arrow ((<<<))
import qualified Data.Time.Calendar as C

newtype MakeExpr haskells wires = MakeExpr (haskells -> E.Expr wires)
newtype MakeMaybeExpr mHaskells mWires = MakeMaybeExpr
                                         (MakeExpr mHaskells mWires)
newtype MakeJustExpr haskells jWires = MakeJustExpr
                                         (MakeExpr haskells jWires)

instance P.Profunctor MakeExpr where
  dimap f g (MakeExpr h) = MakeExpr (P.dimap f (P.dimap id g) h)

-- This instance just does the obvious thing.  Would probably be
-- easier to implement if we gave `ExprArr a` an `Applicative`
-- instance.
instance PP.ProductProfunctor MakeExpr where
  empty = MakeExpr (const PP.empty)
  MakeExpr f ***! MakeExpr g = MakeExpr (fmap (P.dimap (const ((), ())) id
                                               . uncurry (***!))
                                         $ f ***! g)

instance P.Profunctor MakeMaybeExpr where
  dimap f g (MakeMaybeExpr h) = MakeMaybeExpr (P.dimap f g h)

instance PP.ProductProfunctor MakeMaybeExpr where
  empty = MakeMaybeExpr PP.empty
  MakeMaybeExpr f ***! MakeMaybeExpr g = MakeMaybeExpr (f ***! g)

instance P.Profunctor MakeJustExpr where
  dimap f g (MakeJustExpr h) = MakeJustExpr (P.dimap f g h)

instance PP.ProductProfunctor MakeJustExpr where
  empty = MakeJustExpr PP.empty
  MakeJustExpr f ***! MakeJustExpr g = MakeJustExpr (f ***! g)

constant :: Q.ShowConstant a => MakeExpr a (Wire a)
constant = MakeExpr E.constant

constantMaybe :: Q.ShowConstant a => MakeMaybeExpr (Maybe a) (Maybe (Wire a))
constantMaybe = MakeMaybeExpr (MakeExpr (\x -> case x
                                               of Nothing -> A.arr (const Nothing)
                                                  Just a -> A.arr Just
                                                            <<< E.constant a))

constantJust :: Q.ShowConstant a => MakeJustExpr a (Maybe (Wire a))
constantJust = MakeJustExpr (MakeExpr (\a -> A.arr Just <<< E.constant a))

makeExprPP :: MakeExpr haskells wires -> haskells -> E.Expr wires
makeExprPP (MakeExpr f) = f

makeExpr :: D.Default MakeExpr haskells wires => haskells -> E.Expr wires
makeExpr = makeExprPP D.def

makeMaybeExprPP :: MakeMaybeExpr mHaskells mWires -> mHaskells -> E.Expr mWires
makeMaybeExprPP (MakeMaybeExpr (MakeExpr f)) = f

makeMaybeExpr :: D.Default MakeMaybeExpr mHaskells mWires =>
                 mHaskells -> E.Expr mWires
makeMaybeExpr = makeMaybeExprPP D.def

makeJustExprPP :: MakeJustExpr haskells jWires -> haskells -> E.Expr jWires
makeJustExprPP (MakeJustExpr (MakeExpr f)) = f

makeJustExpr :: D.Default MakeJustExpr haskells jWires =>
                 haskells -> E.Expr jWires
makeJustExpr = makeJustExprPP D.def

instance Q.ShowConstant a => D.Default MakeExpr a (Wire a) where
  def = constant

instance Q.ShowConstant a =>
         D.Default MakeMaybeExpr (Maybe a) (Maybe (Wire a)) where
  def = constantMaybe

instance Q.ShowConstant a =>
         D.Default MakeJustExpr a (Maybe (Wire a)) where
  def = constantJust
{-
instance D.Default MakeMaybeExpr (Maybe C.Day) (Maybe (Wire C.Day)) where
  def = MakeMaybeExpr (MakeExpr (\x -> case x
                                       of Nothing -> A.arr (const Nothing)
                                          Just a -> A.arr Just
                                                    <<< E.constantDay a))
-}