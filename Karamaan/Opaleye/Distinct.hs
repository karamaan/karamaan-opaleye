{-# LANGUAGE FlexibleContexts #-}

module Karamaan.Opaleye.Distinct where

import Karamaan.Opaleye.QueryArr (Query, runSimpleQueryArr, simpleQueryArr)
import Karamaan.Opaleye.Wire (Wire(Wire))
import Database.HaskellDB.PrimQuery (PrimQuery(Group),PrimExpr(AttrExpr))
import Karamaan.Opaleye.Operators2 (union)
import Karamaan.Opaleye.QueryColspec (QueryColspec)
import qualified Karamaan.Opaleye.Unpackspec as U
import qualified Data.Profunctor.Product as PP
import qualified Data.Profunctor.Product.Default as D
import Data.Profunctor.Product.Default (Default)

distinct' :: U.Unpackspec wires -> Query wires -> Query wires
distinct' u q = simpleQueryArr $ \((), t0) ->
  let (a, primQ, t1) = runSimpleQueryArr q ((), t0)
      cols = U.runUnpackspec u a
  in (a, Group (map (\oldCol -> (oldCol, AttrExpr oldCol)) cols) primQ, t1)

distinctBetter :: D.Default (PP.PPOfContravariant U.Unpackspec) wires wires =>
                  Query wires -> Query wires
distinctBetter = distinct' D.cdef 

-- I realised you can implement distinct x = x `union` x!
-- This may fail massively with large queries unless the optimiser realises
-- that I'm taking the union of the same query twice.
-- TODO: Try to just implement this as x `union` "empty"?
{-# DEPRECATED distinct "Use 'distinctBetter' instead" #-}
distinct :: Default QueryColspec a a => Query a -> Query a
distinct x = x `union` x

-- This is how I used to implement it.  It didn't work very well.
-- I think this is a correct implementation, but HaskellDB still seems to have
-- trouble dealing with GROUP BY. See Report.Trade.Descendants.activeEdgesBroken
{-# DEPRECATED distinct1 "Use 'distinctBetter' instead" #-}
distinct1 :: Query (Wire a) -> Query (Wire a)
distinct1 q = simpleQueryArr $ \((), t0) ->
  let (Wire oldCol, primQ, t1) = runSimpleQueryArr q ((), t0)
-- vv We used to do
--    newCol = tagWith t1 oldCol
--    t2 = next t1
-- but now we just do
      newCol = oldCol
      t2 = t1
-- and HaskellDB seems happier with that.  Note that 'tagWith t1
-- oldCol' adds an *additional* tag to a column name that presumably
-- already has one.  I wasn't sure if that was a good idea anyway.
  in (Wire newCol, Group [(newCol, AttrExpr oldCol)] primQ, t2)
