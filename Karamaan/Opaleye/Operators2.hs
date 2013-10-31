{-# LANGUAGE Arrows, FlexibleContexts #-}

module Karamaan.Opaleye.Operators2 where

import Karamaan.Opaleye.Wire (Wire(Wire), unWire)
import qualified Karamaan.Opaleye.Wire as Wire
import Karamaan.Opaleye.QueryArr (Query, QueryArr(QueryArr), next, tagWith, Tag,
                                  simpleQueryArr, runSimpleQueryArr)
import Database.HaskellDB.Query (ShowConstant, showConstant)
import Database.HaskellDB.PrimQuery (PrimQuery(Project, Binary),
                                     RelOp(Union, Intersect, Difference), extend,
                                     PrimExpr(AttrExpr, ConstExpr),
                                     BinOp,
                                     UnOp(OpIsNull),
                                     Assoc,
                                     Literal(OtherLit))
import qualified Database.HaskellDB.PrimQuery as PrimQuery
import Karamaan.Opaleye.Operators (binOp')
import qualified Karamaan.Opaleye.Operators as Operators
import Control.Arrow ((***), arr, (<<<), second)
import Data.Time.Calendar (Day)
import qualified Karamaan.Opaleye.Values as Values
import Karamaan.Opaleye.Colspec (Colspec', runWriterOfColspec',
                                 runPackMapOfColspec')
import Karamaan.Opaleye.Default (Default, def)

unOp :: ShowConstant c => BinOp -> String -> String -> c
        -> QueryArr (Wire a) (Wire a)
unOp op opname constname constval = QueryArr f
  where f (w, primQ, t0) = (w', primQ', next t0)
          where s = unWire w
                t_string = s
                t'_string = constname
                t = AttrExpr s
                t' = Operators.constant constval
                (assoc, w') = binOp' op opname t t_string t' t'_string
                                     (tagWith t0)
                primQ' = extend assoc primQ

eq :: QueryArr (Wire a, Wire a) (Wire Bool)
eq = opArr PrimQuery.OpEq "eq"

notEq :: QueryArr (Wire a, Wire a) (Wire Bool)
notEq = opArr PrimQuery.OpNotEq "not_eq"

-- TODO: does HaskellDB support this?  Is it another Postgres incompatibility
-- thing and we should use the Postgres SQL generator explicitly?
cat :: QueryArr (Wire String, Wire String) (Wire String)
cat = opArr (PrimQuery.OpOther "||") "cat"

cat3 :: QueryArr (Wire String, Wire String, Wire String) (Wire String)
cat3 = proc (s1, s2, s3) -> do
  -- TODO: there must be a nicer way of doing this
  s1s2 <- cat -< (s1, s2)
  cat -< (s1s2, s3)

isNull :: QueryArr (Wire (Maybe a)) (Wire Bool)
isNull = unOpArr OpIsNull "is_null"

opArr :: BinOp -> String -> QueryArr (Wire a, Wire a) (Wire b)
opArr op opname = QueryArr f
  where f ((u, u'), primQ, t1) = (newWire, extend newAssoc primQ, next t1)
          where (newAssoc, newWire) = wireBinOp op opname u u' t1

-- TODO: duplication with opArr?
unOpArr :: UnOp -> String -> QueryArr (Wire a) (Wire b)
unOpArr op opname = QueryArr f
  where f (u, primQ, t1) = (newWire, extend newAssoc primQ, next t1)
          where (newAssoc, newWire) = wireUnOp op opname u t1

-- FIXME: what's the right type signature for this?
-- TODO: there's some duplication between this, binOp' and wireOp
wireBinOp :: BinOp -> String -> Wire a -> Wire a -> Tag -> (Assoc, Wire a2)
wireBinOp op opname u u' t1 = binOp' op opname (AttrExpr w) w (AttrExpr w') w'
                                     (tagWith t1)
  where w = unWire u
        w' = unWire u'

-- TODO: some duplication with wireBinOp?
wireUnOp :: UnOp -> String -> Wire a -> Tag -> (Assoc, Wire a2)
wireUnOp op opname u t1 = Operators.unOp op opname (AttrExpr w) w
                                         (tagWith t1)
  where w = unWire u

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

intersect :: Default Colspec' a a =>
             QueryArr () a -> QueryArr () a -> QueryArr () a
intersect = intersect' def

union :: Default Colspec' a a
         => QueryArr () a -> QueryArr () a -> QueryArr () a
union = union' def

difference :: Default Colspec' a a =>
              QueryArr () a -> QueryArr () a -> QueryArr () a
difference = difference' def

intersect' :: Colspec' a b -> QueryArr () a -> QueryArr () a -> QueryArr () b
intersect' = binrel Intersect

union' :: Colspec' a b -> QueryArr () a -> QueryArr () a -> QueryArr () b
union' = binrel Union

difference' :: Colspec' a b -> QueryArr () a -> QueryArr () a -> QueryArr () b
difference' = binrel Difference

-- I tried Query (a, a) a and couldn't get it to work.  Also
-- I guess this would lead to a loss of sharing and much bigger queries.
-- Maybe the optimiser will prune all the uncessary stuff though.
--
binrel :: RelOp -> Colspec' a b -> QueryArr () a -> QueryArr () a
          -> QueryArr () b
binrel op colspec q1 q2 = simpleQueryArr f where
  f ((), t0) = (w_out, primQ, next t2)
    where (w1, primQ1, t1) = runSimpleQueryArr q1 ((), t0)
          (w2, primQ2, t2) = runSimpleQueryArr q2 ((), t1)

          w_out = packMap' (tagWith t2) w1
          -- This used to be
          -- new = unpack w_out
          -- which wasn't well typed when changed to use the new Colspec'
          -- interface.  This implementation is equivalent, but somehow
          -- seems less satisfying.  Should it?
          new = (map (tagWith t2)) (unpack' w1)

          assoc = zip new . map AttrExpr . unpack'

          old1_assoc = assoc w1
          old2_assoc = assoc w2

          r1 :: PrimQuery
          r1 = Project old1_assoc primQ1
          r2 :: PrimQuery
          r2 = Project old2_assoc primQ2

          primQ = Binary op r1 r2

          packMap' = runPackMapOfColspec' colspec
          unpack' = runWriterOfColspec' colspec

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

ifThenElse :: QueryArr (Wire Bool, Wire a, Wire a) (Wire a)
ifThenElse = proc (cond, ifTrue, ifFalse) -> do
  case_ -< ([(cond, ifTrue)], ifFalse)

fromMaybe :: QueryArr (Wire a, Wire (Maybe a)) (Wire a)
fromMaybe = proc (d, m) -> do
  isNull' <- isNull -< m
  ifThenElse -< (isNull', d, Wire.unsafeCoerce m)

fromMaybe' :: Query (Wire a) -> QueryArr (Wire (Maybe a)) (Wire a)
fromMaybe' d = proc m -> do
  d' <- d -< ()
  fromMaybe -< (d', m)

wireToPrimExpr :: Wire a -> PrimExpr
wireToPrimExpr = AttrExpr . unWire

opC :: QueryArr (a, b) c -> Query b -> QueryArr a c
opC op q = op <<< second q <<< arr (\a -> (a, ()))
