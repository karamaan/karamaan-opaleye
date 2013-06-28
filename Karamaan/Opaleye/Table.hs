module Karamaan.Opaleye.Table where

import Karamaan.Opaleye.QueryArr (Query, QueryArr(QueryArr), next, tagWith)
import Database.HaskellDB.PrimQuery (PrimQuery(Project, BaseTable),
                                     PrimExpr(AttrExpr),
                                     Attribute, Assoc, times)
import Control.Arrow ((***))
import Karamaan.Opaleye.Pack (Pack, pack)

makeTable :: Pack a => [String] -> String -> Query a
makeTable x = makeTable' (zip x x)

makeTable' :: Pack a
              => [(String, String)] -> String -> Query a
makeTable' cols table_name = QueryArr f
  where f ((), primQuery, t0) = (retwires, times primQuery primQuery', next t0)
          where (retwires, primQuery') = makeTable'' cols table_name (tagWith t0)

-- TODO: this needs tidying
makeTable'' :: Pack a
               => [(String, String)] -> String -> (String -> String)
               -> (a, PrimQuery)
makeTable'' cols table_name tag' =
  let basetablecols :: [String]
      basetablecols = map snd cols
      makeAssoc :: (String, String) -> (Attribute, PrimExpr)
      makeAssoc = tag' *** AttrExpr
      projcols :: Assoc
      projcols = map makeAssoc cols
      q :: PrimQuery
      q = Project projcols (BaseTable table_name basetablecols)
      retwires = (pack . map fst) projcols
  in (retwires, q)
