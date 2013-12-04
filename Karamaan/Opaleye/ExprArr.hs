module Karamaan.Opaleye.ExprArr where

import Prelude hiding ((.), id, or)
import qualified Data.Map as Map
import Data.Map (Map)
import Database.HaskellDB.PrimQuery (PrimExpr, extend)
import Control.Arrow (Arrow, arr, first, (&&&), (***))
import Control.Category (Category, id, (.), (<<<))
import Karamaan.Opaleye.QueryArr (Tag, first3, next, tagWith, start,
                                  QueryArr(QueryArr))
import Database.HaskellDB.Query (ShowConstant, showConstant)
import Karamaan.Opaleye.Wire (Wire(Wire))
import qualified Database.HaskellDB.PrimQuery as PQ
import qualified Data.Maybe as M
import Karamaan.WhaleUtil.Arrow (replaceWith, foldrArr, opC)
import Karamaan.Opaleye.Operators (operatorName)
import Data.Profunctor (Profunctor, dimap)
import Data.Profunctor.Product (ProductProfunctor, empty, (***!))

-- This is a more type-safe way, and a nicer API, to doing the PrimExpr
-- fiddling that Predicates.hs does.  When there's time we'll convert
-- all of Predicates.hs to this way of doing things.

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

instance Profunctor ExprArr where
  dimap f g a = arr g <<< a <<< arr f

instance ProductProfunctor ExprArr where
  empty = id
  (***!) = (***)

runExprArr :: ExprArr a b -> (a, Scope, Tag) -> (b, Scope, Tag)
runExprArr (ExprArr f) = f

runExprArrStart :: ExprArr a b -> (a, Scope) -> (b, Scope, Tag)
runExprArrStart expr (a, scope) = runExprArr expr (a, scope, start)

runExprArrStartEmpty :: ExprArr a b -> a -> (b, Scope, Tag)
runExprArrStartEmpty expr a = runExprArr expr (a, Map.empty, start)

runExprArr'' :: ExprArr a (Wire b) -> (a, Scope) -> PrimExpr
runExprArr'' expr (a, scope) = unsafeScopeLookup b scope1
  where (b, scope1, _) = runExprArrStart expr (a, scope)

runExprArr' :: Expr (Wire a) -> PrimExpr
runExprArr' = flip runExprArr'' ((), Map.empty)

constant :: ShowConstant a => a -> Expr (Wire a)
constant c = ExprArr g
  where g ((), _, t0) = (w, scope, next t0)
          where ws = tagWith t0 "constant"
                w = Wire ws
                scope = Map.singleton ws (PQ.ConstExpr (showConstant c))

-- TODO: Could probably do something like
-- makeOp :: opMaker a b -> (b -> PrimExpr) -> ExprArr a b -> Wire c
-- where makeOp is a ProductProfunctor
-- This would allow us to do constants (nullary operations), unary operations
-- and binary operations uniformly, providing a bit more type safety due to
-- parametricity.
-- TODO: ExprArr (Wire a, Wire b) (Wire c)?
binOp :: PQ.BinOp -> String -> ExprArr (Wire a, Wire a) (Wire b)
binOp op name = ExprArr g
  where g ((u, u'), scope, t0) = (w, scope', next t0)
          where ws = tagWith t0 (operatorName v name v')
                w = Wire ws
                (Wire v, Wire v') = (u, u')
                lookupS = flip unsafeScopeLookup scope
                uExpr = lookupS u
                u'Expr = lookupS u'
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

-- TODO: can make some ProductProfunctors for this:
-- queryOfExprIn qIn eIn -> queryOfExprOut qOut eOut -> ExprArr eIn eOut
--   -> QueryArr qIn qOut
toQueryArr11 :: ExprArr (Wire a) (Wire b) -> QueryArr (Wire a) (Wire b)
toQueryArr11 exprArr = QueryArr f
  where f (w0, primQ0, t0) = (Wire w1, primQ1, t1)
          where (Wire w1, scope1, t1) = runExprArr exprArr (w0, scope0, t0)
                -- TODO: Is this w0 tagged?
                -- Perhaps it is, as we assume it comes out of a query.
                scope0 = scopeOfWire w0
                expr = unsafeScopeLookup (Wire w1) scope1
                primQ1 = extend [(w1, expr)] primQ0

scopeOfWire :: Wire a -> Scope
scopeOfWire (Wire s) = Map.singleton s (PQ.AttrExpr s)

unsafeScopeLookup :: Wire a -> Scope -> PrimExpr
unsafeScopeLookup (Wire w) s = M.fromJust (Map.lookup w s)
