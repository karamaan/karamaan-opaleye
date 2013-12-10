{-# LANGUAGE FlexibleContexts #-}

module Karamaan.Opaleye.Table where

import Karamaan.Opaleye.QueryArr (Query, next, tagWith, simpleQueryArr)
import Database.HaskellDB.PrimQuery (PrimQuery(Project, BaseTable),
                                     PrimExpr(AttrExpr),
                                     Attribute, Assoc)
import Karamaan.Opaleye.TableColspec (TableColspec, TableColspecP,
                                      runWriterOfColspec, runPackMapOfColspec,
                                      tableColspecOfTableColspecP,
                                      WireMaker, runWireMaker)
import Karamaan.Opaleye.Default (Default, def)
import Control.Arrow ((***))
import Karamaan.WhaleUtil ((.:))

-- For specifying the columns as Strings
data TableSpec a = TableSpec a String

-- For specifying the columns as Wires
data Table a = Table String a

tableOfTableSpec :: WireMaker a b -> TableSpec a -> Table b
tableOfTableSpec wireMaker (TableSpec cols name) = Table name wireCols
  where wireCols = runWireMaker wireMaker cols

makeTableSpecDef :: (Default WireMaker a b, Default TableColspecP b b)
                    => TableSpec a -> Query b
makeTableSpecDef (TableSpec cols name) = makeTableDef cols name

-- For typeclass resolution it seems best to force the arguments to be
-- the same.  Users can always use makeTableT to get more flexibility
-- if they want.
makeTableTDef :: Default TableColspecP a a => Table a -> Query a
makeTableTDef = makeTableT def

-- makeTableDef is informally deprecated.  Use makeTableTDef instead
-- I would formally deprecate it with a pragma but we still have a lot
-- of code that uses it.
makeTableDef :: (Default WireMaker a b, Default TableColspecP b b)
                => a -> String -> Query b
makeTableDef = makeTableTDef . tableOfTableSpec def .: TableSpec

makeTableT :: TableColspecP a b -> Table a -> Query b
makeTableT colspec (Table name cols) = makeTableQueryColspec colspec cols name

makeTableQueryColspec :: TableColspecP a b -> a -> String -> Query b
makeTableQueryColspec = makeTable .: tableColspecOfTableColspecP

-- makeTable is informally deprecated, but Values.hs still uses it,
-- so I don't want to deprecate it with a pragma yet.
--{-# DEPRECATED makeTable "Use 'makeTableT' or 'makeTableTDef' instead" #-}
makeTable :: TableColspec a -> String -> Query a
makeTable colspec = makeTable' colspec (zip x x)
  where x = runWriterOfColspec colspec

makeTable' :: TableColspec a -> [(String, String)] -> String -> Query a
makeTable' colspec cols table_name = simpleQueryArr f
  where f ((), t0) = (retwires, primQuery, next t0)
          where (retwires, primQuery) = makeTable'' colspec cols table_name (tagWith t0)

-- TODO: this needs tidying
makeTable'' :: TableColspec a -> [(String, String)] -> String -> (String -> String)
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
