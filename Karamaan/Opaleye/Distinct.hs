{-# LANGUAGE FlexibleContexts #-}

module Karamaan.Opaleye.Distinct where

import Karamaan.Opaleye.QueryArr (Query, runSimpleQueryArr, simpleQueryArr)
import Karamaan.Opaleye.Wire (Wire(Wire))
import Database.HaskellDB.PrimQuery (PrimQuery(Group),PrimExpr(AttrExpr))
import Karamaan.Opaleye.Operators2 (union)
import Karamaan.Opaleye.QueryColspec (QueryColspec)
import Karamaan.Opaleye.Default (Default)

-- TODO: now that we have more experience with product profunctors we
-- can probably use a ProductProfunctor to do generic DISTINCT!

-- I realised you can implement distinct x = x `union` x!
-- This may fail massively with large queries unless the optimiser realises
-- that I'm taking the union of the same query twice.
-- TODO: Try to just implement this as x `union` "empty"?
distinct :: Default QueryColspec a a => Query a -> Query a
distinct x = x `union` x

-- This is how I used to implement it.  It didn't work very well.
-- I think this is a correct implementation, but HaskellDB still seems to have
-- trouble dealing with GROUP BY. See Report.Trade.Descendants.activeEdgesBroken
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
