{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module Karamaan.Opaleye.MakeExpr where

import qualified Karamaan.Opaleye.ExprArr as E
import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP
import Data.Profunctor.Product ((***!))
import qualified Karamaan.Opaleye.ShowConstant as S
import Karamaan.Opaleye.Wire (Wire)
import qualified Data.Profunctor.Product.Default as D
import qualified Control.Arrow as A
import Control.Arrow ((<<<))

{- MakeExpr provides conveniences for turning Haskell values into
   Opaleye `Expr`s.

   As with Karamaan.Opaleye.ShowConstant, explicit type signatures
   will be needed frequently.
-}

newtype MakeExpr haskells wires = MakeExpr (haskells -> E.Expr wires)
newtype MakeMaybeExpr mHaskells mWires = MakeMaybeExpr
                                         (MakeExpr mHaskells mWires)
newtype MakeJustExpr haskells jWires = MakeJustExpr
                                         (MakeExpr haskells jWires)

-- {
-- These instances are all completely boilerplate.  In principle they
-- could be derived.

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

-- End of boilerplate instances
-- }


constant :: S.ShowConstant a b => MakeExpr a (Wire b)
constant = MakeExpr S.showConstant

constantMaybe :: S.ShowConstant a b => MakeMaybeExpr (Maybe a) (Maybe (Wire b))
constantMaybe = MakeMaybeExpr (MakeExpr constantMaybe')
  where constantMaybe' :: S.ShowConstant a b
                          => Maybe a -> E.Expr (Maybe (Wire b))
        constantMaybe' Nothing = A.arr (const Nothing)
        constantMaybe' (Just a) = A.arr Just <<< S.showConstant a

constantJust :: S.ShowConstant a b => MakeJustExpr a (Maybe (Wire b))
constantJust = MakeJustExpr (MakeExpr constantJust')
  where constantJust' :: S.ShowConstant a b
                         => a -> E.Expr (Maybe (Wire b))
        constantJust' a = A.arr Just <<< S.showConstant a

makeExprPP :: MakeExpr haskells wires -> haskells -> E.Expr wires
makeExprPP (MakeExpr f) = f

-- The following typeclass functions have the example specialisations,
-- assuming
--
--   1. the necessary correspondence between a and a', etc, as given by
--      ShowConstant
--   2. a suitable Default instance for the MyRecord type

-- makeExpr :: MyRecord a b c -> Expr (MyRecord (Wire a') (Wire b') (Wire c'))
--
-- This is used for converting Haskell values to Opaleye Exprs
makeExpr :: D.Default MakeExpr haskells wires => haskells -> E.Expr wires
makeExpr = makeExprPP D.def

makeMaybeExprPP :: MakeMaybeExpr mHaskells mWires -> mHaskells -> E.Expr mWires
makeMaybeExprPP (MakeMaybeExpr (MakeExpr f)) = f

-- makeMaybeExpr :: MyRecord (Maybe a) (Maybe b) (Maybe c)
--                  -> Expr (MyRecord (Maybe (Wire a'))
--                                    (Maybe (Wire b'))
--                                    (Maybe (Wire c')))
--
-- This is used for the data manipulation functionality
makeMaybeExpr :: D.Default MakeMaybeExpr mHaskells mWires =>
                 mHaskells -> E.Expr mWires
makeMaybeExpr = makeMaybeExprPP D.def

makeJustExprPP :: MakeJustExpr haskells jWires -> haskells -> E.Expr jWires
makeJustExprPP (MakeJustExpr (MakeExpr f)) = f

-- makeMaybeExpr :: MyRecord a b c
--                  -> Expr (MyRecord (Maybe (Wire a'))
--                                    (Maybe (Wire b'))
--                                    (Maybe (Wire c')))
--
-- This is used for the data manipulation functionality
makeJustExpr :: D.Default MakeJustExpr haskells jWires =>
                 haskells -> E.Expr jWires
makeJustExpr = makeJustExprPP D.def

instance S.ShowConstant a b => D.Default MakeExpr a (Wire b) where
  def = constant

instance S.ShowConstant a b =>
         D.Default MakeMaybeExpr (Maybe a) (Maybe (Wire b)) where
  def = constantMaybe

instance S.ShowConstant a b =>
         D.Default MakeJustExpr a (Maybe (Wire b)) where
  def = constantJust
