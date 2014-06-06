{-# LANGUAGE Arrows, FlexibleContexts, MultiParamTypeClasses #-}

module Karamaan.Opaleye.Operators2 where

import Prelude hiding (and, or, not)
import Karamaan.Opaleye.Wire (Wire(Wire), unWire)
import Karamaan.Opaleye.OperatorsPrimatives (binrel)
import Karamaan.Opaleye.QueryArr (Query, QueryArr(QueryArr), next, tagWith)
import Database.HaskellDB.Query (ShowConstant, showConstant)
import Database.HaskellDB.PrimQuery (RelOp(Union, Intersect, Difference, UnionAll),
                                     extend,
                                     PrimExpr(AttrExpr),
                                     Literal(OtherLit))
import qualified Database.HaskellDB.PrimQuery as PrimQuery
import Control.Arrow ((***), Arrow, (&&&), (<<<), second)
import Control.Applicative (Applicative, pure, (<*>))
import Data.Time.Calendar (Day)
import qualified Karamaan.Opaleye.Values as Values
import Karamaan.Opaleye.QueryColspec (QueryColspec)
import Data.Profunctor.Product.Default (Default, def)
import qualified Karamaan.Opaleye.ExprArr as E
import Karamaan.Plankton.Arrow (replaceWith)
import qualified Karamaan.Plankton.Arrow as A
import Karamaan.Plankton.Arrow.ReaderCurry (readerCurry2)
import Data.Profunctor (Profunctor, dimap)
import Data.Profunctor.Product (ProductProfunctor, (***!), empty, defaultEmpty,
                                defaultProfunctorProduct)

-- The only reason this is called Operators2 rather than Operators is that
-- I had to split the Operators module in two to avoid circular dependencies.
-- At some point I should come up with a better naming system.

eq :: QueryArr (Wire a, Wire a) (Wire Bool)
eq = E.toQueryArrDef E.eq

and :: QueryArr (Wire Bool, Wire Bool) (Wire Bool)
and = E.toQueryArrDef E.and

or :: QueryArr (Wire Bool, Wire Bool) (Wire Bool)
or = E.toQueryArrDef E.or

not :: QueryArr (Wire Bool) (Wire Bool)
not = E.toQueryArrDef E.not

notEq :: QueryArr (Wire a, Wire a) (Wire Bool)
notEq = E.toQueryArrDef E.notEq

doesntEqualAnyOf :: ShowConstant a => [a] -> QueryArr (Wire a) (Wire Bool)
doesntEqualAnyOf xs = not <<< equalsOneOf xs

equalsOneOf :: ShowConstant a => [a] -> QueryArr (Wire a) (Wire Bool)
equalsOneOf = E.toQueryArrDef . E.equalsOneOf

-- TODO: HaskellDB's 'cat' or '.++.' is implemented as SQL's '+' even when
-- using the PostgreSQL generator.  The correct fix is probably to fix
-- the PostgreSQL generator (Database.HaskellDB.Sql.PostgreSQL).
cat :: QueryArr (Wire String, Wire String) (Wire String)
cat = E.toQueryArrDef E.cat

constantLit :: Literal -> Query (Wire a)
constantLit = E.toQueryArrDef . E.constantLit

-- TODO: is this type signature right?
-- Doesn't seem to work for string with postgresql-simple
-- because postgresql-simple seems to need a type sig on its strings
constant :: ShowConstant a => a -> Query (Wire a)
constant = constantLit . showConstant

-- Postgres seems to need type signatures on constant strings
constantString :: String -> Query (Wire String)
constantString = unsafeConstant . ("'" ++) . (++"' :: text")

-- HaskellDB doesn't have a ShowConstant instance for Day, only for
-- CalendarTime from old-time.  We could perhaps just add an orphan
-- instance.
constantDay :: Day -> Query (Wire Day)
constantDay = unsafeConstant . Values.dayToSQL

unsafeConstant :: String -> Query (Wire a)
unsafeConstant = constantLit . OtherLit

-- Note that the natural type for 'intersect' and 'union' would be
-- ... => QueryArr (a, a) a.  However I am not sure what it means to
-- implement such a signature in SQL.  I don't know if SQL is limited
-- so that such a thing would not make sense, or whether my
-- understanding of what is necessary is insufficient.

type RelOpT a b = QueryArr () a -> QueryArr () a -> QueryArr () b

intersect :: Default QueryColspec a a => RelOpT a a
intersect = intersect' def

union :: Default QueryColspec a a => RelOpT a a
union = union' def

unionAll :: Default QueryColspec a a => RelOpT a a
unionAll = unionAll' def

difference :: Default QueryColspec a a => RelOpT a a
difference = difference' def

intersect' :: QueryColspec a b -> RelOpT a b
intersect' = binrel Intersect

union' :: QueryColspec a b -> RelOpT a b
union' = binrel Union

unionAll' :: QueryColspec a b -> RelOpT a b
unionAll' = binrel UnionAll

difference' :: QueryColspec a b -> RelOpT a b
difference' = binrel Difference

-- Case stuff

type CaseArg a = ([(Wire Bool, a)], a)

fmapCaseArg :: (a -> b) -> CaseArg a -> CaseArg b
fmapCaseArg f = map (second f) *** f

newtype CaseRunner a b = CaseRunner (QueryArr (CaseArg a) b)

instance Profunctor CaseRunner where
  dimap f g (CaseRunner q) = CaseRunner (dimap (fmapCaseArg f) g q)

instance Functor (CaseRunner a) where
  fmap f (CaseRunner c) = CaseRunner (fmap f c)

instance Applicative (CaseRunner a) where
  pure = CaseRunner . pure
  CaseRunner f <*> CaseRunner x = CaseRunner (f <*> x)

instance ProductProfunctor CaseRunner where
  empty = defaultEmpty
  (***!) = defaultProfunctorProduct

instance Default CaseRunner (Wire a) (Wire a) where
  def = CaseRunner case_

runCase :: CaseRunner a b -> QueryArr (CaseArg a) b
runCase (CaseRunner q) = q

caseDef :: Default CaseRunner a b => QueryArr (CaseArg a) b
caseDef = runCase def

case_ :: QueryArr ([(Wire Bool, Wire a)], Wire a) (Wire a)
case_ = QueryArr f where
  f ((cases, otherwise_), primQ, t0) = (w_out, primQ', t1)
    where t1 = next t0
          attrname_out = tagWith t0 "case_result"
          w_out = Wire attrname_out
          cases' = map (wireToPrimExpr *** wireToPrimExpr) cases
          otherwise' = wireToPrimExpr otherwise_
          caseExpr = PrimQuery.CaseExpr cases' otherwise'
          primQ' = extend [(attrname_out, caseExpr)] primQ

-- End of case stuff

ifThenElse :: Default CaseRunner a b
              => QueryArr (Wire Bool, a, a) b
ifThenElse = proc (cond, ifTrue, ifFalse) ->
  caseDef -< ([(cond, ifTrue)], ifFalse)

wireToPrimExpr :: Wire a -> PrimExpr
wireToPrimExpr = AttrExpr . unWire

-- ReaderCurried versions

-- TODO: What is the right way to be polymorphic over SQL's numeric types?
-- Num is not really appropriate (though it's what HaskellDB chose)
type NumBinOpG a b = NumBinOp2G a b b
type NumBinOp2G a b c = QueryArr a (Wire b) -> QueryArr a (Wire b)
                        -> QueryArr a (Wire c)

-- A short name since we will be using it a lot.  Probably not a good idea to
-- import this into application code, though!
r :: QueryArr (Wire b, Wire b) (Wire c) -> NumBinOp2G a b c
r = readerCurry2

constantRC :: ShowConstant b => b -> QueryArr a (Wire b)
constantRC = replaceWith . constant

(.==.) :: NumBinOp2G a b Bool
(.==.) = r eq

(./=.) :: NumBinOp2G a b Bool
(./=.) = r notEq

(.&&.) :: NumBinOpG a Bool
(.&&.) = r and

(.||.) :: NumBinOpG a Bool
(.||.) = r or

(.++.) :: NumBinOp2G a String String
(.++.) = r cat

-- TODO: this signature could now be generalised to something involving
-- CaseRunner
ifThenElseRC :: QueryArr t (Wire Bool)
                -> QueryArr t (Wire a)
                -> QueryArr t (Wire a)
                -> QueryArr t (Wire a)
ifThenElseRC cond ifTrue ifFalse = proc a -> do
  cond' <- cond -< a
  ifTrue' <- ifTrue -< a
  ifFalse' <- ifFalse -< a

  ifThenElse -< (cond', ifTrue', ifFalse')

caseRC :: [(QueryArr a (Wire Bool), QueryArr a (Wire b))]
          -> QueryArr a (Wire b) -> QueryArr a (Wire b)
caseRC cases else_ = case_ <<< (cases' &&& else_)
  where cases' = A.traverseArr (uncurry (&&&)) cases
