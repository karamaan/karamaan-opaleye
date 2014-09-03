{-# LANGUAGE FlexibleContexts #-}

module Karamaan.Opaleye.Distinct where

import Karamaan.Opaleye.QueryArr (Query, runSimpleQueryArr, simpleQueryArr, Tag)
import Karamaan.Opaleye.Wire (Wire(Wire))
import Database.HaskellDB.PrimQuery (PrimQuery(Group),PrimExpr(AttrExpr))
import Karamaan.Opaleye.Operators2 (union)
import Karamaan.Opaleye.QueryColspec (QueryColspec)
import qualified Karamaan.Opaleye.Unpackspec as U
import qualified Data.Profunctor.Product as PP
import qualified Data.Profunctor.Product.Default as D
import Data.Profunctor.Product.Default (Default)

{-
Design comment: There is a neater way of doing `distinct` that would
avoid getting our hands dirty with explicit PrimQuery constructors but
would introduce a new product profunctor.  That is, create a product
profunctor which makes an aggregator all of whose components are `groupBy`

NB: This could only work if HaskellDB actually did GROUP BY properly.
As it is if you GROUP BY all columns it is equivalent to the identity
operation.  We can only use this design comment once we have our own
AST, at which point we could just introduce our own DISTINCT operation
anyway.
-}

distinctBetter :: D.Default (PP.PPOfContravariant U.Unpackspec) wires wires =>
                  Query wires -> Query wires
distinctBetter = distinct' D.cdef

distinct' :: U.Unpackspec wires -> Query wires -> Query wires
distinct' unpack q = simpleQueryArr (distinctU' unpack . runSimpleQueryArr q)

distinctU' :: U.Unpackspec wires -> (wires, PrimQuery, Tag)
              -> (wires, PrimQuery, Tag)
distinctU' unpack (wires, primQ, t) = (wires, primQ', t)
  where cols = U.runUnpackspec unpack wires
        primQ' = Group (map (\col -> (col, AttrExpr col)) cols) primQ

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
