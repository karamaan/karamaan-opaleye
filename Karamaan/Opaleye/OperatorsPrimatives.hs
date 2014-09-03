module Karamaan.Opaleye.OperatorsPrimatives where

import Karamaan.Opaleye.QueryArr (QueryArr, next, tagWith,
                                  simpleQueryArr, runSimpleQueryArr)
import Database.HaskellDB.PrimQuery (PrimExpr(AttrExpr),
                                     PrimQuery(Project, Binary), RelOp)
import Karamaan.Opaleye.QueryColspec (QueryColspec, runWriterOfQueryColspec,
                                      runPackMapOfQueryColspec)

-- This should be merged with Operators.hs
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
          -- A solution would be to augment QueryColspec with a
          -- generalization of runPackMap that can tag with increasing
          -- tags, rather than just a fixed one.  I think this would
          -- be simplest to achieve if we made runPackMap a mapM
          -- rather than a map (or in lens terminology a traversal).
          --
          -- (Two wires in w1 will have the same name, for example, if
          -- one wire is duplicated, say via 'id &&& id')
          new = map tag' (runWriter w1)

          oldAssoc = Project . zip new . map AttrExpr . runWriter

          r1 :: PrimQuery
          r1 = oldAssoc w1 primQ1
          r2 :: PrimQuery
          r2 = oldAssoc w2 primQ2

          primQ = Binary op r1 r2

          runPackMap = runPackMapOfQueryColspec colspec
          runWriter = runWriterOfQueryColspec colspec
