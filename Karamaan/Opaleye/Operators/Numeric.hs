module Karamaan.Opaleye.Operators.Numeric where

import qualified Karamaan.Opaleye.ExprArr as E
import Karamaan.Opaleye.Operators2 (NumBinOpG, NumBinOp2G, r)
import Karamaan.Opaleye.OperatorsPrimatives (unOp)
import Karamaan.Opaleye.Wire (Wire)
import Karamaan.Opaleye.QueryArr (QueryArr)
import Database.HaskellDB.PrimQuery(BinOp(OpMul, OpOther))

-- FIXME: the type signatures are odd here.  We pass in an Int for the sake of
-- avoiding an ambiguous type variable, but then we return Wire a when perhaps
-- it should be Num a => ... Wire a.  I guess the timesArrC should take a Double,
-- and return Num a whereas the mod should take only an Int and return Ints.
--
-- Think about this some more ...
type NumBinOp a = Int -> QueryArr (Wire a) (Wire a)

type NumBinOpH a b = QueryArr (Wire a, Wire a) (Wire b)

type NumUnOp a = QueryArr (Wire a) (Wire a)

timesC :: NumBinOp a
timesC x = unOp OpMul "times" (show x) x

-- HaskellDB's OpMod comes out as "x MOD y" which Postgres doesn't like
-- TODO: the solution to this is to make sure we use the correct SQL
-- generator.  See
-- http://hackage.haskell.org/packages/archive/haskelldb/2.2.2/doc/html/src/Database-HaskellDB-Sql-PostgreSQL.html#generator
modC :: NumBinOp a
modC x = unOp (OpOther "%") "mod" (show x) x

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
