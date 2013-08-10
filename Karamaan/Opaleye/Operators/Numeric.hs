module Karamaan.Opaleye.Operators.Numeric where

import Karamaan.Opaleye.Operators2 (unOp, opArr)
import Karamaan.Opaleye.Wire (Wire)
import Karamaan.Opaleye.QueryArr (QueryArr)
import Database.HaskellDB.PrimQuery(BinOp(OpMul, OpDiv, OpPlus, OpMinus,
                                          OpOther))

import qualified Database.HaskellDB.PrimQuery as PrimQuery

-- FIXME: the type signatures are odd here.  We pass in an Int for the sake of
-- avoiding an ambiguous type variable, but then we return Wire a when perhaps
-- it should be Num a => ... Wire a.  I guess the timesArrC should take a Double,
-- and return Num a whereas the mod should take only an Int and return Ints.
--
-- Think about this some more ...
type NumBinOp a = Int -> QueryArr (Wire a) (Wire a)

timesC :: NumBinOp a
timesC x = unOp OpMul "times" (show x) x

-- HaskellDB's OpMod comes out as "x MOD y" which Postgres doesn't like
-- TODO: the solution to this is to make sure we use the correct SQL
-- generator.  See
-- http://hackage.haskell.org/packages/archive/haskelldb/2.2.2/doc/html/src/Database-HaskellDB-Sql-PostgreSQL.html#generator
modC :: NumBinOp a
modC x = unOp (OpOther "%") "mod" (show x) x

-- It's also unclear what types these operations should have
plus :: QueryArr (Wire a, Wire a) (Wire a)
plus = opArr OpPlus "plus"

divide :: QueryArr (Wire a, Wire a) (Wire a)
divide = opArr OpDiv "div"

times :: QueryArr (Wire a, Wire a) (Wire a)
times = opArr OpMul "times"

minus :: QueryArr (Wire a, Wire a) (Wire a)
minus = opArr OpMinus "minus"

gt :: QueryArr (Wire a, Wire a) (Wire Bool)
gt = opArr PrimQuery.OpGt "gt"

gte :: QueryArr (Wire a, Wire a) (Wire Bool)
gte = opArr PrimQuery.OpGtEq "gteq"
