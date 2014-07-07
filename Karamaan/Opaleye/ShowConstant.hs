{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Karamaan.Opaleye.ShowConstant where

import qualified Karamaan.Opaleye.ExprArr as E
import qualified Karamaan.Opaleye.Nullable as N
import qualified Database.HaskellDB.PrimQuery as PQ
import Karamaan.Opaleye.Wire (Wire)
import Control.Arrow ((<<<))
import Data.Time.Calendar (Day)

{- The ShowConstant class is intended to eventualy replace HaskellDB's
   ShowConstant.

   It needs to be a MPTC because the type Haskell-side might not be
   the same as the type Opaleye-side.  For example, we will have an
   instance `ShowConstant Text String`.  The drawback to this is that
   explicit type signatures will be needed more frequently.

   If you have a sensible ShowConstant instance you are welcome to add
   it here!
-}

class ShowConstant haskell opaleye where
  showConstant :: haskell -> E.Expr (Wire opaleye)

instance ShowConstant haskell opaleye
         => ShowConstant (Maybe haskell) (N.Nullable opaleye) where
         showConstant Nothing = N.null
         showConstant (Just a) = N.toNullableExpr <<< showConstant a

instance ShowConstant String String where
  showConstant = E.constantLit . PQ.StringLit

instance ShowConstant Int Int where
  showConstant = E.constantLit . PQ.IntegerLit . fromIntegral

instance ShowConstant Integer Integer where
  showConstant = E.constantLit . PQ.IntegerLit

instance ShowConstant Double Double where
  showConstant = E.constantLit . PQ.DoubleLit

instance ShowConstant Bool Bool where
  showConstant = E.constantLit . PQ.BoolLit

instance ShowConstant Day Day where
  showConstant = E.constantDay
