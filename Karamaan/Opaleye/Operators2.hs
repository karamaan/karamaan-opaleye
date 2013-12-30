{-# LANGUAGE Arrows, FlexibleContexts, MultiParamTypeClasses #-}

module Karamaan.Opaleye.Operators2 where

import Prelude hiding (and, or, not)
import Karamaan.Opaleye.Wire (Wire(Wire), unWire)
import qualified Karamaan.Opaleye.Wire as Wire
import Karamaan.Opaleye.OperatorsPrimatives (opArr, unOpArr, binrel)
import Karamaan.Opaleye.QueryArr (Query, QueryArr(QueryArr), next, tagWith)
import Database.HaskellDB.Query (ShowConstant, showConstant)
import Database.HaskellDB.PrimQuery (RelOp(Union, Intersect, Difference), extend,
                                     PrimExpr(AttrExpr, ConstExpr),
                                     UnOp(OpIsNull),
                                     Literal(OtherLit))
import qualified Database.HaskellDB.PrimQuery as PrimQuery
import Control.Arrow ((***), Arrow, (&&&), (<<<), second)
import Control.Applicative (Applicative, pure, (<*>))
import Data.Time.Calendar (Day)
import qualified Karamaan.Opaleye.Values as Values
import Karamaan.Opaleye.QueryColspec (QueryColspec)
import Data.Profunctor.Product.Default (Default, def)
import qualified Karamaan.Opaleye.ExprArr as E
import Karamaan.WhaleUtil.Arrow (replaceWith, foldrArr)
import qualified Karamaan.WhaleUtil.Arrow as A
import Karamaan.WhaleUtil.Arrow.ReaderCurry (readerCurry2)
import Data.Profunctor (Profunctor, dimap)
import Data.Profunctor.Product (ProductProfunctor, (***!), empty, defaultEmpty,
                                defaultProfunctorProduct)


-- The only reason this is called Operators2 rather than Operators is that
-- I had to split the Operators module in two to avoid circular dependencies.
-- At some point I should come up with a better naming system.

eq :: QueryArr (Wire a, Wire a) (Wire Bool)
eq = opArr PrimQuery.OpEq "eq"

and :: QueryArr (Wire Bool, Wire Bool) (Wire Bool)
and = opArr PrimQuery.OpAnd "and"

or :: QueryArr (Wire Bool, Wire Bool) (Wire Bool)
or = opArr PrimQuery.OpOr "or"

not :: QueryArr (Wire Bool) (Wire Bool)
not = unOpArr PrimQuery.OpNot "not"

notEq :: QueryArr (Wire a, Wire a) (Wire Bool)
notEq = opArr PrimQuery.OpNotEq "not_eq"

doesntEqualAnyOf :: ShowConstant a => [a] -> QueryArr (Wire a) (Wire Bool)
-- TODO: Should this be foldl', since laziness gets us nothing here?
doesntEqualAnyOf = foldrArr and true . map (opC notEq . constant)
  where true = replaceWith (constant True)
-- vv Want to do this, but you will not be surprised to hear that
-- there is a bug in HaskellDB's query optimizer
-- doesntEqualAnyOf xs = Karamaan.Opaleye.Operators2.not <<< equalsOneOf xs

{-# WARNING brokenHaskellDB "brokenHaskellDB: DO NOT USE.  This is only for\
            \pointing out a HaskellDB brokenness" #-}
-- Try 'showSqlForPostgresDefault brokenHaskellDB' in GHCi
brokenHaskellDB :: Query (Wire Bool)
brokenHaskellDB = not <<< or <<< (constant False &&& constant False)

equalsOneOf :: ShowConstant a => [a] -> QueryArr (Wire a) (Wire Bool)
equalsOneOf = E.toQueryArr11 . E.equalsOneOf

-- TODO: HaskellDB's 'cat' or '.++.' is implemented as SQL's '+' even when
-- using the PostgreSQL generator.  The correct fix is probably to fix
-- the PostgreSQL generator (Database.HaskellDB.Sql.PostgreSQL).
cat :: QueryArr (Wire String, Wire String) (Wire String)
cat = opArr (PrimQuery.OpOther "||") "cat"

{-# DEPRECATED cat3 "Better to use ReaderCurried operations" #-}
cat3 :: QueryArr (Wire String, Wire String, Wire String) (Wire String)
cat3 = proc (s1, s2, s3) -> do
  -- TODO: there must be a nicer way of doing this
  s1s2 <- cat -< (s1, s2)
  cat -< (s1s2, s3)

{-# DEPRECATED isNull "Use 'Karamaan.Opaleye.Nullable.isNull'" #-}
isNull :: QueryArr (Wire (Maybe a)) (Wire Bool)
isNull = unOpArr OpIsNull "is_null"

constantLit :: Literal -> Query (Wire a)
constantLit l = QueryArr f where
  f ((), primQ, t0) = (w, primQ', next t0)
    where primQ' = extend [(ws, ConstExpr l)] primQ
          ws = tagWith t0 "constant"
          w = Wire ws

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
intersect :: Default QueryColspec a a =>
             QueryArr () a -> QueryArr () a -> QueryArr () a
intersect = intersect' def

union :: Default QueryColspec a a
         => QueryArr () a -> QueryArr () a -> QueryArr () a
union = union' def

difference :: Default QueryColspec a a =>
              QueryArr () a -> QueryArr () a -> QueryArr () a
difference = difference' def

intersect' :: QueryColspec a b -> QueryArr () a -> QueryArr () a -> QueryArr () b
intersect' = binrel Intersect

union' :: QueryColspec a b -> QueryArr () a -> QueryArr () a -> QueryArr () b
union' = binrel Union

difference' :: QueryColspec a b -> QueryArr () a -> QueryArr () a -> QueryArr () b
difference' = binrel Difference

-- Case stuff

type CaseArg a = ([(Wire Bool, a)], a)

fmapCaseArg :: (a -> b) -> CaseArg a -> CaseArg b
fmapCaseArg f = (map (second f) *** f)

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

{-# DEPRECATED fromMaybe "Use 'Karamaan.Opaleye.Nullable.fromNullable'\
    \instead" #-}
fromMaybe :: QueryArr (Wire a, Wire (Maybe a)) (Wire a)
fromMaybe = proc (d, m) -> do
  isNull' <- isNull -< m
  ifThenElse -< (isNull', d, Wire.unsafeCoerce m)

{-# DEPRECATED fromMaybe' "Use 'Karamaan.Opaleye.Nullable.fromNullable''\
    \instead" #-}
fromMaybe' :: Query (Wire a) -> QueryArr (Wire (Maybe a)) (Wire a)
fromMaybe' d = proc m -> do
  d' <- d -< ()
  fromMaybe -< (d', m)

wireToPrimExpr :: Wire a -> PrimExpr
wireToPrimExpr = AttrExpr . unWire

{-# DEPRECATED opC "Use 'Karamaan.WhaleUtil.Arrow.opC' instead" #-}
opC :: Arrow arr => arr (a, b) c -> arr () b -> arr a c
opC = A.opC

-- ReaderCurried versions

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
