module Karamaan.Opaleye.ExprArr where

import Prelude hiding ((.), id, or)
import qualified Data.Map as Map
import Data.Map (Map)
import Database.HaskellDB.PrimQuery (PrimExpr, extend)
import Control.Arrow (Arrow, arr, first, (&&&))
import Control.Category (Category, id, (.), (<<<))
import Karamaan.Opaleye.QueryArr (Tag, first3, next, tagWith, start,
                                  QueryArr(QueryArr))
import Database.HaskellDB.Query (ShowConstant, showConstant)
import Karamaan.Opaleye.Wire (Wire(Wire))
import qualified Database.HaskellDB.PrimQuery as PQ
import qualified Data.Maybe as M
import Karamaan.WhaleUtil.Arrow (replaceWith, foldrArr, opC)
import Karamaan.Opaleye.Operators (operatorName)

type Scope = Map String PrimExpr

newtype ExprArr a b = ExprArr ((a, Scope, Tag) -> (b, Scope, Tag))

type Expr b = ExprArr () b

instance Category ExprArr where
  id = ExprArr id
  ExprArr g . ExprArr f = ExprArr (g . f)

instance Arrow ExprArr where
  arr f = ExprArr (first3 f)
  first f = ExprArr g
    where g ((a, z), scope, t0) = ((b, z), scope' `Map.union` scope, t1)
            where (b, scope', t1) = runExprArr f (a, scope, t0)

runExprArr :: ExprArr a b -> (a, Scope, Tag) -> (b, Scope, Tag)
runExprArr (ExprArr f) = f

runExprArr' :: Expr (Wire a) -> PrimExpr
runExprArr' expr = M.fromJust (Map.lookup a scope)
  where (Wire a, scope, _) = runExprArr expr ((), Map.empty, start)

constant :: ShowConstant a => a -> Expr (Wire a)
constant c = ExprArr g
  where g ((), _, t0) = (w, scope, next t0)
          where ws = tagWith t0 "constant"
                w = Wire ws
                scope = Map.singleton ws (PQ.ConstExpr (showConstant c))

-- TODO: ExprArr (Wire a, Wire b) (Wire c)?
binOp :: PQ.BinOp -> String -> ExprArr (Wire a, Wire a) (Wire b)
binOp op name = ExprArr g
  where g ((u, u'), scope, t0) = (w, scope', next t0)
          where ws = tagWith t0 (operatorName v name v')
                w = Wire ws
                (Wire v, Wire v') = (u, u')
                -- Naughty fromJust!
                lookupS = M.fromJust . flip Map.lookup scope
                uExpr = lookupS v
                u'Expr = lookupS v'
                scope' = Map.singleton ws (PQ.BinExpr op uExpr u'Expr)

plus :: ExprArr (Wire a, Wire a) (Wire a)
plus = binOp PQ.OpPlus "plus"

mul :: ExprArr (Wire a, Wire a) (Wire a)
mul = binOp PQ.OpMul "mul"

or :: ExprArr (Wire Bool, Wire Bool) (Wire Bool)
or = binOp PQ.OpOr "or"

eq :: ExprArr (Wire a, Wire a) (Wire Bool)
eq = binOp PQ.OpEq "eq"

one :: Expr (Wire Int)
one = constant 1

two :: Expr (Wire Int)
two = constant 2

three :: Expr (Wire Int)
three = constant 3

onePlusTwo :: Expr (Wire Int)
onePlusTwo = plus <<< (one &&& two)

sumTimesThree :: Expr (Wire Int)
sumTimesThree = mul <<< (three &&& onePlusTwo)

equalsOneOf :: ShowConstant a => [a] -> ExprArr (Wire a) (Wire Bool)
-- TODO: Should this be foldl', since laziness gets us nothing here?
equalsOneOf = foldrArr or false . map (opC eq . constant)
  where false = replaceWith (constant False)

toQueryArr11 :: ExprArr (Wire a) (Wire b) -> QueryArr (Wire a) (Wire b)
toQueryArr11 exprArr = QueryArr f
  where f (Wire w0, primQ0, t0) = (Wire w1, primQ1, t1)
          where (Wire w1, scope1, t1) = runExprArr exprArr (Wire w0, scope0, t0)
                scope0 = Map.singleton w0 (PQ.AttrExpr w0)
                expr = M.fromJust (Map.lookup w1 scope1)
                primQ1 = extend [(w1, expr)] primQ0

equalsOneOfQ :: ShowConstant a => [a] -> QueryArr (Wire a) (Wire Bool)
equalsOneOfQ = toQueryArr11 . equalsOneOf
