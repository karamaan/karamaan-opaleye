module Karamaan.Opaleye.OperatorsPrimatives where

import Karamaan.Opaleye.Wire (Wire, unWire)
import Karamaan.Opaleye.QueryArr (QueryArr(QueryArr), next, tagWith, Tag)
import Database.HaskellDB.Query (ShowConstant)
import Database.HaskellDB.PrimQuery (extend,
                                     PrimExpr(AttrExpr),
                                     UnOp,
                                     BinOp,
                                     Assoc)
import qualified Karamaan.Opaleye.Operators as Operators
import Karamaan.Opaleye.Operators (binOp')

-- This could perhaps be merged with Operators, but really it all
-- needs tidying up anyway.

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
