{-# LANGUAGE FlexibleContexts, DeriveFunctor #-}

module Karamaan.Opaleye.ExprArr
    ( Scope
    , ExprArr
    , Expr
    , runExprArr''
    , scopeOfWire
    , runExprArrStartEmpty
    , runExprArrStart
    , emptyScope
    , unsafeScopeLookup
    , unsafeCoerce
    , mapUnion
    , eq
    , plus
    , mul
    , constant
    , or
    , toQueryArrDef
    , and
    , not
    , constantLit
    , constantDay
    , equalsOneOf
    , cat
    , notEq
    , unOp
    , lt
    , lte
    , gt
    , gte
    , mod
    , abs
    , divide
    , times
    , minus
    ) where

import Control.Applicative (Applicative (..))
import Prelude hiding ((.), id, or, and, not, mod, abs)
import qualified Data.Map as Map
import Data.Map (Map)
import Database.HaskellDB.PrimQuery (PrimExpr, extend, Literal)
import Control.Arrow (Arrow, arr, first, (***), (&&&))
import Control.Category (Category, id, (.), (<<<))
import Karamaan.Opaleye.QueryArr (Tag, first3, next, tagWith, start,
                                  QueryArr(QueryArr))
import Database.HaskellDB.Query (ShowConstant, showConstant)
import Karamaan.Opaleye.Wire (Wire(Wire))
import qualified Karamaan.Opaleye.Wire as Wire
import qualified Database.HaskellDB.PrimQuery as PQ
import Karamaan.Plankton.Arrow (replaceWith, foldrArr, opC)
import Karamaan.Opaleye.Operators (operatorName)
import Data.Profunctor (Profunctor, dimap)
import Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import Data.Time.Calendar (Day)
import qualified Karamaan.Opaleye.Values as Values
import qualified Karamaan.Opaleye.Unpackspec as U
import qualified Data.Profunctor.Product as P
import qualified Data.Profunctor.Product.Default as D
import qualified Data.List as List

-- This is a more type-safe way, and a nicer API, to doing the PrimExpr
-- fiddling that Predicates.hs does.  When there's time we'll convert
-- all of Predicates.hs to this way of doing things.

type Scope = Map String PrimExpr

emptyScope :: Scope
emptyScope = Map.empty

newtype ExprArr a b = ExprArr ((a, Scope, Tag) -> (b, Scope, Tag))
  deriving (Functor)

instance Applicative (ExprArr a) where
  pure = arr . const
  f <*> x = arr (uncurry ($)) . (f &&& x)

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
runExprArrStartEmpty expr a = runExprArr expr (a, emptyScope, start)

runExprArr'' :: ExprArr a (Wire b) -> (a, Scope) -> PrimExpr
runExprArr'' expr (a, scope) = unsafeScopeLookup b scope1
  where (b, scope1, _) = runExprArrStart expr (a, scope)

-- Note that the returned Scope value is what is *added* to the
-- overall scope, so ignoring the incoming Scope here is not a bug!
constantLit :: Literal -> Expr (Wire a)
constantLit l = ExprArr g
  where g ((), _, t0) = (w, scope, next t0)
          where ws = tagWith t0 "constant"
                w = Wire ws
                scope = Map.singleton ws (PQ.ConstExpr l)

constant :: ShowConstant a => a -> Expr (Wire a)
constant = constantLit . showConstant

constantDay :: Day -> Expr (Wire Day)
constantDay = unsafeConstant . Values.dayToSQL

unsafeConstant :: String -> Expr (Wire a)
unsafeConstant = constantLit . PQ.OtherLit

unsafeCoerce :: ExprArr (Wire a) (Wire b)
unsafeCoerce = arr Wire.unsafeCoerce

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

unOp :: PQ.UnOp -> String -> ExprArr (Wire a) (Wire b)
unOp op name = ExprArr g
  where g (u, scope, t0) = (w, scope', next t0)
          where ws = tagWith t0 (name ++ "_" ++ take 5 v)
                w = Wire ws
                Wire v = u
                lookupS = flip unsafeScopeLookup scope
                uExpr = lookupS u
                scope' = Map.singleton ws (PQ.UnExpr op uExpr)

abs :: ExprArr (Wire a) (Wire a)
abs = unOp (PQ.UnOpOther "@") "abs"

plus :: ExprArr (Wire a, Wire a) (Wire a)
plus = binOp PQ.OpPlus "plus"

minus :: ExprArr (Wire a, Wire a) (Wire a)
minus = binOp PQ.OpMinus "minus"

-- TODO: deprecate this one
mul :: ExprArr (Wire a, Wire a) (Wire a)
mul = binOp PQ.OpMul "mul"

times :: ExprArr (Wire a, Wire a) (Wire a)
times = binOp PQ.OpMul "times"

divide :: ExprArr (Wire a, Wire a) (Wire a)
divide = binOp PQ.OpDiv "div"

-- HaskellDB's OpMod comes out as "x MOD y" which Postgres doesn't like
-- TODO: the solution to this is to make sure we use the correct SQL
-- generator.  See
-- http://hackage.haskell.org/packages/archive/haskelldb/2.2.2/doc/html/src/Database-HaskellDB-Sql-PostgreSQL.html#generator
mod :: ExprArr (Wire a, Wire a) (Wire a)
mod = binOp (PQ.OpOther "%") "mod"

gt :: ExprArr (Wire a, Wire a) (Wire Bool)
gt = binOp PQ.OpGt "gt"

gte :: ExprArr (Wire a, Wire a) (Wire Bool)
gte = binOp PQ.OpGtEq "gte"

lt :: ExprArr (Wire a, Wire a) (Wire Bool)
lt = binOp PQ.OpLt "lt"

lte :: ExprArr (Wire a, Wire a) (Wire Bool)
lte = binOp PQ.OpLtEq "lte"

or :: ExprArr (Wire Bool, Wire Bool) (Wire Bool)
or = binOp PQ.OpOr "or"

and :: ExprArr (Wire Bool, Wire Bool) (Wire Bool)
and = binOp PQ.OpAnd "and"

not :: ExprArr (Wire Bool) (Wire Bool)
not = unOp PQ.OpNot "not"

eq :: ExprArr (Wire a, Wire a) (Wire Bool)
eq = binOp PQ.OpEq "eq"

notEq :: ExprArr (Wire a, Wire a) (Wire Bool)
notEq = binOp PQ.OpNotEq "noteq"

cat :: ExprArr (Wire String, Wire String) (Wire String)
cat = binOp (PQ.OpOther "||") "cat"

equalsOneOf :: ShowConstant a => [a] -> ExprArr (Wire a) (Wire Bool)
-- TODO: Should this be foldl', since laziness gets us nothing here?
equalsOneOf = foldrArr or false . map (opC eq . constant)
  where false = replaceWith (constant False)

toQueryArr :: U.Unpackspec a -> U.Unpackspec b -> ExprArr a b -> QueryArr a b
toQueryArr writera writerb exprArr = QueryArr f
  where f (w0, primQ0, t0) = (w1, primQ1, t1)
          where scope0 = scopeOfCols writera w0
                (w1, scope1, t1) = runExprArr exprArr (w0, scope0, t0)
                exprs = (map (\w -> (w, unsafeScopeLookup (Wire w) scope1))
                         . U.runUnpackspec writerb) w1
                primQ1 = extend exprs primQ0

mapUnion :: Ord k => [Map k v] -> Map k v
mapUnion = List.foldl' Map.union Map.empty

toQueryArrDef :: (D.Default (P.PPOfContravariant U.Unpackspec) a a,
                  D.Default (P.PPOfContravariant U.Unpackspec) b b)
                 => ExprArr a b -> QueryArr a b
toQueryArrDef = toQueryArr (P.unPPOfContravariant D.def)
                           (P.unPPOfContravariant D.def)

scopeOfWire :: Wire a -> Scope
scopeOfWire (Wire s) = Map.singleton s (PQ.AttrExpr s)

scopeOfCols :: U.Unpackspec wires -> wires -> Scope
scopeOfCols unpackspec = mapUnion
                         . map (scopeOfWire . Wire)
                         . U.runUnpackspec unpackspec

unsafeScopeLookup :: Wire a -> Scope -> PrimExpr
unsafeScopeLookup (Wire w) s = case Map.lookup w s of
  Nothing -> error ("Could not find Wire " ++ w ++ " in scope")
  Just a  -> a
