module Karamaan.Opaleye.Table where

import Karamaan.Opaleye.QueryArr (Query, next, tagWith, simpleQueryArr)
import Database.HaskellDB.PrimQuery (PrimQuery(Project, BaseTable),
                                     PrimExpr(AttrExpr),
                                     Attribute, Assoc)
import Karamaan.Opaleye.Colspec (Colspec, runWriterOfColspec,
                                 runPackMapOfColspec)
import Control.Arrow ((***))


makeTable :: Colspec a -> String -> Query a
makeTable colspec = makeTable' colspec (zip x x)
  where x = runWriterOfColspec colspec

makeTable' :: Colspec a -> [(String, String)] -> String -> Query a
makeTable' colspec cols table_name = simpleQueryArr f
  where f ((), t0) = (retwires, primQuery, next t0)
          where (retwires, primQuery) = makeTable'' colspec cols table_name (tagWith t0)

-- TODO: this needs tidying
makeTable'' :: Colspec a -> [(String, String)] -> String -> (String -> String)
               -> (a, PrimQuery)
makeTable'' colspec cols table_name tag' =
  let basetablecols :: [String]
      basetablecols = map snd cols
      makeAssoc :: (String, String) -> (Attribute, PrimExpr)
      makeAssoc = tag' *** AttrExpr
      projcols :: Assoc
      projcols = map makeAssoc cols
      q :: PrimQuery
      q = Project projcols (BaseTable table_name basetablecols)
  in (runPackMapOfColspec colspec tag', q)
