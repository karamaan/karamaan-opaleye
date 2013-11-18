module Karamaan.Opaleye.Operators.Numeric where

import Karamaan.Opaleye.Operators2 (unOp, opArr, constant)
import Karamaan.Opaleye.Wire (Wire)
import Karamaan.Opaleye.QueryArr (QueryArr)
import Database.HaskellDB.PrimQuery(BinOp(OpMul, OpDiv, OpPlus, OpMinus,
                                          OpOther))
import Control.Category ((<<<))
import Control.Arrow ((&&&), arr, Arrow)
import Database.HaskellDB.Query (ShowConstant)

import qualified Database.HaskellDB.PrimQuery as PrimQuery

-- FIXME: the type signatures are odd here.  We pass in an Int for the sake of
-- avoiding an ambiguous type variable, but then we return Wire a when perhaps
-- it should be Num a => ... Wire a.  I guess the timesArrC should take a Double,
-- and return Num a whereas the mod should take only an Int and return Ints.
--
-- Think about this some more ...
type NumBinOp a = Int -> QueryArr (Wire a) (Wire a)

type NumBinOpH a b = QueryArr (Wire a, Wire a) (Wire b)

timesC :: NumBinOp a
timesC x = unOp OpMul "times" (show x) x

-- HaskellDB's OpMod comes out as "x MOD y" which Postgres doesn't like
-- TODO: the solution to this is to make sure we use the correct SQL
-- generator.  See
-- http://hackage.haskell.org/packages/archive/haskelldb/2.2.2/doc/html/src/Database-HaskellDB-Sql-PostgreSQL.html#generator
modC :: NumBinOp a
modC x = unOp (OpOther "%") "mod" (show x) x

-- It's also unclear what types these operations should have
-- Should there be a Num typeclass constraint or similar?
plus :: NumBinOpH a a
plus = opArr OpPlus "plus"

divide :: NumBinOpH a a
divide = opArr OpDiv "div"

times :: NumBinOpH a a
times = opArr OpMul "times"

minus :: NumBinOpH a a
minus = opArr OpMinus "minus"

gt :: NumBinOpH a Bool
gt = opArr PrimQuery.OpGt "gt"

gte :: NumBinOpH a Bool
gte = opArr PrimQuery.OpGtEq "gte"

lt :: NumBinOpH a Bool
lt = opArr PrimQuery.OpLt "lt"

lte :: NumBinOpH a Bool
lte = opArr PrimQuery.OpLtEq "lte"

type NumBinOpG a b = NumBinOp2G a b b
type NumBinOp2G a b c = QueryArr a (Wire b) -> QueryArr a (Wire b) -> QueryArr a (Wire c)

opG :: Arrow arr => arr (a, b) r -> arr z a -> arr z b -> arr z r
opG op f g = op <<< (f &&& g)

(.+.) :: NumBinOpG a b
(.+.) = opG plus

(.*.) :: NumBinOpG a b
(.*.) = opG times

(./.) :: NumBinOpG a b
(./.) = opG divide

(.-.) :: NumBinOpG a b
(.-.) = opG minus

(.<.) :: NumBinOp2G a Bool Bool
(.<.) = opG lt

(.>.) :: NumBinOp2G a Bool Bool
(.>.) = opG gt

(.<=.) :: NumBinOp2G a Bool Bool
(.<=.) = opG lte

(.>=.) :: NumBinOp2G a Bool Bool
(.>=.) = opG gte

-- There's no good reason this is called 'G'
constantG :: ShowConstant b => b -> QueryArr a (Wire b)
constantG a = constant a <<< (arr (const ()))
