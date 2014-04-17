module Karamaan.Opaleye.OperatorsPrimatives where

import Karamaan.Opaleye.Wire (Wire, unWire)
import Karamaan.Opaleye.QueryArr (QueryArr(QueryArr), next, tagWith, Tag,
                                  simpleQueryArr, runSimpleQueryArr)
import Database.HaskellDB.Query (ShowConstant)
import Database.HaskellDB.PrimQuery (extend,
                                     PrimExpr(AttrExpr),
                                     UnOp,
                                     BinOp,
                                     Assoc,
                                     PrimQuery(Project, Binary), RelOp)
import qualified Karamaan.Opaleye.Operators as Operators
import Karamaan.Opaleye.Operators (binOp')
import Karamaan.Opaleye.QueryColspec (QueryColspec, runWriterOfQueryColspec,
                                      runPackMapOfQueryColspec)

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

unOpArr :: UnOp -> String -> QueryArr (Wire a) (Wire b)
unOpArr op opname = QueryArr f
  where f (u, primQ, t1) = (newWire, extend newAssoc primQ, next t1)
          where (newAssoc, newWire) = wireUnOp op opname u t1

wireUnOp :: UnOp -> String -> Wire a -> Tag -> (Assoc, Wire a2)
wireUnOp op opname u t1 = Operators.unOp op opname (AttrExpr w) w
                                         (tagWith t1)
  where w = unWire u

binrel :: RelOp -> QueryColspec a b -> QueryArr () a -> QueryArr () a
          -> QueryArr () b
binrel op colspec q1 q2 = simpleQueryArr f where
  f ((), t0) = (w_out, primQ, next t2)
    where (w1, primQ1, t1) = runSimpleQueryArr q1 ((), t0)
          (w2, primQ2, t2) = runSimpleQueryArr q2 ((), t1)

          tag' :: String -> String
          tag' = tagWith t2

          w_out = runPackMap tag' w1
          -- This used to be
          -- new = unpack w_out
          -- which wasn't well typed when changed to use the new QueryColspec
          -- interface.  This implementation is equivalent, but somehow
          -- seems less satisfying.  Should it?
          --
          -- FIXME: Note that there is a bug here.  If two of the wires in w1
          -- have the same name then they will have the same name in new.
          -- This leads to a select of the form
          -- select w1name as w1nametag, w1name as w1nametag, ...
          -- which is an error as w1nametag is ambiguous.
          --
          -- A solution would be to augment QueryColspec with a generalization
          -- of runPackMap that can tag with increasing tags, rather than
          -- just a fixed one.
          --
          -- Later note: I can no longer see why I thought it was
          -- possible that two wires in w1 should be able to have the
          -- same name.  A wire in w1 can have the same name as a wire
          -- in w2, but that's not the same thing at all!  Is this
          -- FIXME actually just not a problem?  Tom -- 2013-12-18
          new = map tag' (runWriter w1)

          assoc = zip new . map AttrExpr . runWriter

          old1_assoc = assoc w1
          old2_assoc = assoc w2

          r1 :: PrimQuery
          r1 = Project old1_assoc primQ1
          r2 :: PrimQuery
          r2 = Project old2_assoc primQ2

          primQ = Binary op r1 r2

          runPackMap = runPackMapOfQueryColspec colspec
          runWriter = runWriterOfQueryColspec colspec
