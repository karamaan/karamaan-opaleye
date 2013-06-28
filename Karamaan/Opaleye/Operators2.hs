module Karamaan.Opaleye.Operators2 where

import Karamaan.Opaleye.Wire (Wire(Wire), unWire)
import Karamaan.Opaleye.QueryArr (Query, QueryArr(QueryArr), next, tagWith, Tag,
                                  runQueryArr)
import Database.HaskellDB.Query (ShowConstant, showConstant)
import Database.HaskellDB.PrimQuery (PrimQuery(Project, Binary,
                                               Empty),
                                     RelOp(Union), extend,
                                     PrimExpr(AttrExpr, ConstExpr),
                                     BinOp(OpPlus, OpDiv, OpMul, OpOther,
                                           OpMinus),
                                     Assoc,
                                     Literal(OtherLit))
import qualified Database.HaskellDB.PrimQuery as PrimQuery
import Karamaan.Opaleye.Operators (binOp')
import qualified Karamaan.Opaleye.Operators as Operators
import Karamaan.Opaleye.Pack (Pack, packMap, unpack)

unOp :: ShowConstant c => BinOp -> String -> String -> c
        -> QueryArr (Wire a) (Wire a)
unOp op opname constname constval = QueryArr f
  where f (w, primQ, t0) = (w', primQ', next t0)
          where s = unWire w
                t_string = s
                t'_string = constname
                t = AttrExpr s
                -- FIXME: vv sort out the types properly
                -- so this type sig doesn't have to be
                -- specified
                t' = Operators.constant constval
                (assoc, w') = binOp' op opname t t_string t' t'_string
                                     (tagWith t0)
                primQ' = extend assoc primQ

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
modC :: NumBinOp a
modC x = unOp (OpOther "%") "mod" (show x) x

-- It's also unclear what types these operations should have
plus :: QueryArr (Wire a, Wire a) (Wire a)
plus = opArr OpPlus "plus"

divide :: QueryArr (Wire a, Wire a) (Wire a)
divide = opArr OpDiv "div"

minus :: QueryArr (Wire a, Wire a) (Wire a)
minus = opArr OpMinus "minus"

opArr :: BinOp -> String -> QueryArr (Wire a, Wire a) (Wire a)
opArr op opname = QueryArr f
  where f ((u, u'), primQ, t1) = (newWire, extend newAssoc primQ, next t1)
          where (newAssoc, newWire) = wireBinOp op opname u u' t1

-- FIXME: what's the right type signature for this?
-- TODO: there's some duplication between this, binOp' and wireOp
wireBinOp :: BinOp -> String -> Wire a -> Wire a -> Tag -> (Assoc, Wire a2)
wireBinOp op opname u u' t1 = binOp' op opname (AttrExpr w) w (AttrExpr w') w'
                                     (tagWith t1)
  where w = unWire u
        w' = unWire u'

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

unsafeConstant :: String -> Query (Wire a)
unsafeConstant = constantLit . OtherLit

union :: Pack b => QueryArr () b -> QueryArr () b -> QueryArr () b
union = binrel Union

-- I tried Query (a, a) a and couldn't get it to work.  Also
-- I guess this would lead to a loss of sharing and much bigger queries.
-- Maybe the optimiser will prune all the uncessary stuff though.
binrel :: Pack b => RelOp -> QueryArr () b -> QueryArr () b -> QueryArr () b
binrel op q1 q2 = QueryArr f where
  f ((), primQ, t0) = (w_out, PrimQuery.times primQ primQ', next t2)
  -- I didn't know what to do with primQ here, so I just copied
  -- aggregate and timesed it onto primQ'
    where (w1, primQ1, t1) = runQueryArr q1 ((), Empty, t0)
          (w2, primQ2, t2) = runQueryArr q2 ((), Empty, t1)

          old1 = unpack w1
          old2 = unpack w2

          w_out = packMap (tagWith t2) w1
          new = unpack w_out

          old1_assoc = zip new (map AttrExpr old1)
          old2_assoc = zip new (map AttrExpr old2)

          r1 :: PrimQuery
          r1 = Project old1_assoc primQ1
          r2 :: PrimQuery
          r2 = Project old2_assoc primQ2

          primQ' = Binary op r1 r2
