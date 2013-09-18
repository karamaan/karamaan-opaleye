module Karamaan.Opaleye.Distinct where

import Karamaan.Opaleye.QueryArr (Query, QueryArr(QueryArr), runQueryArr, next,
                                  tagWith)
import Karamaan.Opaleye.Wire (Wire(Wire))
import Database.HaskellDB.PrimQuery (PrimQuery(Group, Empty),PrimExpr(AttrExpr),
                                     times)

-- I think this is a correct implementation, but HaskellDB still seems to have
-- trouble dealing with GROUP BY. See Report.Trade.Descendants.activeEdgesBroken
distinct1 :: Query (Wire a) -> Query (Wire a)
distinct1 q = QueryArr $ \((), primQuery, t0) ->
  let (Wire oldCol, primQ, t1) = runQueryArr q ((), Empty, t0)
      -- vv This adds an *additional* tag to a column name that
      -- presumably already has one.  Is this right and/or desirable?
      newCol = tagWith t1 oldCol
      queryMapper = times primQuery . Group [(newCol, AttrExpr oldCol)]
  in (Wire newCol, queryMapper primQ, next t1)
