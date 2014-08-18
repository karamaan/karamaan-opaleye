{-# LANGUAGE FlexibleContexts #-}

module Karamaan.Opaleye.Table where

import Karamaan.Opaleye.QueryArr (Query, next, tagWith, simpleQueryArr)
import Database.HaskellDB.PrimQuery (PrimQuery(Project, BaseTable),
                                     PrimExpr(AttrExpr), Assoc)
import Karamaan.Opaleye.TableColspec (TableColspec, TableColspecP,
                                      runWriterOfColspec, runPackMapOfColspec,
                                      tableColspecOfTableColspecP,
                                      WireMaker, runWireMaker)
import Data.Profunctor.Product.Default (Default, def)
import Control.Arrow ((&&&))
import Karamaan.Plankton ((.:))

-- For specifying the columns as Strings
data TableSpec strings = TableSpec strings String

-- For specifying the columns as Wires
data Table wires = Table String wires

tableOfTableSpec :: WireMaker strings wires -> TableSpec strings -> Table wires
tableOfTableSpec wireMaker (TableSpec cols name) = Table name wireCols
  where wireCols = runWireMaker wireMaker cols

tableOfTableSpecDef :: Default WireMaker strings wires =>
                       TableSpec strings -> Table wires
tableOfTableSpecDef = tableOfTableSpec def

-- For typeclass resolution it seems best to force the arguments to be
-- the same.  Users can always use makeTableT to get more flexibility
-- if they want.
queryTable :: Default TableColspecP wires wires => Table wires -> Query wires
queryTable = makeTableT def

-- I don't know if this should be deprecated or not.  Should we force
-- everything to go through a Table?
makeTableDef :: (Default WireMaker strings wires,
                 Default TableColspecP wires wires) =>
                strings -> String -> Query wires
makeTableDef = queryTable . tableOfTableSpec def .: TableSpec

makeTableT :: TableColspecP wires wires' -> Table wires -> Query wires'
makeTableT colspec (Table name cols) = makeTableQueryColspec colspec cols name

makeTableQueryColspec :: TableColspecP wires wires' ->
                         wires -> String -> Query wires'
makeTableQueryColspec = makeTable .: tableColspecOfTableColspecP

-- makeTable is informally deprecated, but Values.hs still uses it,
-- so I don't want to deprecate it with a pragma yet.
--{-# DEPRECATED makeTable "Use 'makeTableT' or 'queryTable' instead" #-}
makeTable :: TableColspec wires -> String -> Query wires
makeTable = makeTable'

makeTable' :: TableColspec wires -> String -> Query wires
makeTable' colspec tableName = simpleQueryArr f
  where f ((), t0) = (retwires, primQ, next t0)
          where (retwires, primQ) = makeTable'' colspec tableName (tagWith t0)

makeTable'' :: TableColspec wires
               -> String -> (String -> String)
               -> (wires, PrimQuery)
makeTable'' colspec tableName tag' = (wires, primQ)
  where cols = runWriterOfColspec colspec
        projcols :: Assoc
        projcols = map (tag' &&& AttrExpr) cols
        primQ :: PrimQuery
        primQ = Project projcols (BaseTable tableName cols)
        wires = runPackMapOfColspec colspec tag'
