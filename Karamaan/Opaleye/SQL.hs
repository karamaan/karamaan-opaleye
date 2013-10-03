module Karamaan.Opaleye.SQL where

import Database.HaskellDB.Optimize (optimize)
import Database.HaskellDB.Sql.Generate (sqlQuery)
import Database.HaskellDB.Sql.Print (ppSql)
import Database.HaskellDB.Sql.Default (defaultSqlGenerator)
import Database.HaskellDB.PrimQuery (PrimQuery)
import Karamaan.Opaleye.QueryArr (Query, runQueryArrPrim, runQueryArrPrim')
import Karamaan.Opaleye.Pack (Pack)
import Karamaan.Opaleye.Unpackspec (Unpackspec)
import Karamaan.WhaleUtil ((.:))

showSqlForPostgreSQLSimple' :: Unpackspec a -> Query a -> String
showSqlForPostgreSQLSimple' = optimizeFormatAndShowSQL .: runQueryArrPrim'

-- FIXME: vv Once RunQuery is done, this one should go,
-- leaving only the one above
showSqlForPostgreSQLSimple :: Pack a => Query a -> String
showSqlForPostgreSQLSimple = optimizeFormatAndShowSQL . runQueryArrPrim

showSqlUnopt :: Pack a => Query a -> String
showSqlUnopt  = formatAndShowSQL . runQueryArrPrim

formatAndShowSQL :: PrimQuery -> String
formatAndShowSQL = show
                   . ppSql
                   . sqlQuery defaultSqlGenerator

optimizeFormatAndShowSQL :: PrimQuery -> String
optimizeFormatAndShowSQL = formatAndShowSQL . optimize
