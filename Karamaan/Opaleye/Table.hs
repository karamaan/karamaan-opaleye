module Karamaan.Opaleye.Table where

import Karamaan.Opaleye.QueryArr (Query, QueryArr(QueryArr), next, tagWith)
import Database.HaskellDB.PrimQuery (PrimQuery(Project, BaseTable),
                                     PrimExpr(AttrExpr),
                                     Attribute, Assoc, times)
import Karamaan.Opaleye.Colspec (Colspec(Colspec))
import Control.Arrow ((***))


makeTable :: Colspec a -> String -> Query a
makeTable colspec@(Colspec a w) = makeTable' colspec (zip x x)
  where x = w a

makeTable' :: Colspec a -> [(String, String)] -> String -> Query a
makeTable' colspec cols table_name = QueryArr f
  where f ((), primQuery, t0) = (retwires, times primQuery primQuery', next t0)
          where (retwires, primQuery') = makeTable'' colspec cols table_name (tagWith t0)

-- TODO: this needs tidying
makeTable'' :: Colspec a -> [(String, String)] -> String -> (String -> String)
               -> (a, PrimQuery)
makeTable'' (Colspec a _) cols table_name tag' =
  let basetablecols :: [String]
      basetablecols = map snd cols
      makeAssoc :: (String, String) -> (Attribute, PrimExpr)
      makeAssoc = tag' *** AttrExpr
      projcols :: Assoc
      projcols = map makeAssoc cols
      q :: PrimQuery
      q = Project projcols (BaseTable table_name basetablecols)
  in (a, q)
