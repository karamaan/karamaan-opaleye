{-# LANGUAGE Arrows #-}

module Karamaan.Opaleye.Operators.Numeric where

import qualified Karamaan.Opaleye.ExprArr as E
import Karamaan.Opaleye.Operators2 (NumBinOpG, NumBinOp2G, r)
import qualified Karamaan.Opaleye.Operators2 as O2
import Karamaan.Opaleye.Wire (Wire)
import qualified Karamaan.Opaleye.Nullable as N
import Karamaan.Opaleye.QueryArr (QueryArr)
import Control.Arrow ((<<<))

type NumBinOp a = Int -> QueryArr (Wire a) (Wire a)

type NumBinOpH a b = QueryArr (Wire a, Wire a) (Wire b)

type NumUnOp a = QueryArr (Wire a) (Wire a)

-- {
-- FIXME: the type signatures are odd here.  We pass in an Int for the sake of
-- avoiding an ambiguous type variable, but then we return Wire a when perhaps
-- it should be Num a => ... Wire a.  I guess the timesArrC should take a Double,
-- and return Num a whereas the mod should take only an Int and return Ints.
--
-- timesC and modC are just weird anyway and should probably be deprecated.
timesC :: NumBinOp a
timesC x = proc y -> do
  x' <- N.unsafeCoerce <<< O2.constant x -< ()
  times -< (y, x')

modC :: NumBinOp a
modC x = proc y -> do
  x' <- N.unsafeCoerce <<< O2.constant x -< ()
  E.toQueryArrDef E.mod -< (y, x')
-- }

abs :: NumUnOp a
abs = E.toQueryArrDef E.abs

-- It's also unclear what types these operations should have
-- Should there be a Num typeclass constraint or similar?
plus :: NumBinOpH a a
plus = E.toQueryArrDef E.plus

divide :: NumBinOpH a a
divide = E.toQueryArrDef E.divide

times :: NumBinOpH a a
times = E.toQueryArrDef E.times

minus :: NumBinOpH a a
minus = E.toQueryArrDef E.minus

gt :: NumBinOpH a Bool
gt = E.toQueryArrDef E.gt

gte :: NumBinOpH a Bool
gte = E.toQueryArrDef E.gte

lt :: NumBinOpH a Bool
lt = E.toQueryArrDef E.lt

lte :: NumBinOpH a Bool
lte = E.toQueryArrDef E.lte

(.+.) :: NumBinOpG a b
(.+.) = r plus

(.*.) :: NumBinOpG a b
(.*.) = r times

(./.) :: NumBinOpG a b
(./.) = r divide

(.-.) :: NumBinOpG a b
(.-.) = r minus

(.<.) :: NumBinOp2G a b Bool
(.<.) = r lt

(.>.) :: NumBinOp2G a b Bool
(.>.) = r gt

(.<=.) :: NumBinOp2G a b Bool
(.<=.) = r lte

(.>=.) :: NumBinOp2G a b Bool
(.>=.) = r gte
