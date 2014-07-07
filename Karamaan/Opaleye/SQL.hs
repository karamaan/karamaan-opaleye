{-# LANGUAGE FlexibleContexts #-}

module Karamaan.Opaleye.SQL where

import Database.HaskellDB.Optimize (optimize)
import Database.HaskellDB.Sql.Generate (sqlQuery)
import qualified Database.HaskellDB.Sql.Generate as G
import Database.HaskellDB.Sql.Print (ppSql)
import qualified Database.HaskellDB.Sql.Print as P
import Database.HaskellDB.Sql.Default (defaultSqlGenerator)
import Database.HaskellDB.PrimQuery (PrimQuery)
import qualified Database.HaskellDB.PrimQuery as PQ
import Karamaan.Opaleye.QueryArr (Query, runQueryArrPrim)
import Karamaan.Opaleye.Unpackspec (Unpackspec)
import Karamaan.Plankton ((.:))
import Data.Profunctor.Product (PPOfContravariant, unPPOfContravariant)
import Data.Profunctor.Product.Default (Default, def)

-- Currently we only support SQL generation for Postgres because,
-- for example, 'cat' is implemented as '||' and the hackery we do
-- in, for example, Values.hs, may be Postgres specific.
--
-- Support for other DBMSes can be added if required.
showSqlForPostgres :: Unpackspec wires -> Query wires -> String
showSqlForPostgres = optimizeFormatAndShowSQL .: runQueryArrPrim

formatAndShowSQL :: PrimQuery -> String
formatAndShowSQL = show . ppSql . sqlQuery defaultSqlGenerator

formatAndShowSQLExpr :: PQ.PrimExpr -> String
formatAndShowSQLExpr = show . P.ppSqlExpr . G.sqlExpr defaultSqlGenerator

optimizeFormatAndShowSQL :: PrimQuery -> String
optimizeFormatAndShowSQL = formatAndShowSQL . optimize

-- TODO: the other "Default" functions are called "...Def".  I think we
-- should standardize on the latter.
showSqlForPostgresDefault :: Default (PPOfContravariant Unpackspec) wires wires
                             => Query wires -> String
showSqlForPostgresDefault = showSqlForPostgres (unPPOfContravariant def)
