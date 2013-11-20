{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
    ScopedTypeVariables #-}
-- TODO: ^^ get rid of these extensions when we move the default instance
-- somewhere more sensible

module Karamaan.Opaleye.SQL where

import Database.HaskellDB.Optimize (optimize)
import Database.HaskellDB.Sql.Generate (sqlQuery)
import Database.HaskellDB.Sql.Print (ppSql)
import Database.HaskellDB.Sql.Default (defaultSqlGenerator)
import Database.HaskellDB.PrimQuery (PrimQuery)
import Karamaan.Opaleye.QueryArr (Query, runQueryArrPrim)
import Karamaan.Opaleye.Unpackspec (Unpackspec(Unpackspec))
import Karamaan.Opaleye.QueryColspec (writerWire)
import Karamaan.WhaleUtil ((.:))
import Karamaan.Opaleye.Default (Default, def)
import Data.Profunctor.Product (PPOfContravariant(PPOfContravariant))
import Karamaan.Opaleye.Wire (Wire)

{-# DEPRECATED showSqlForPostgreSQLSimple'
    "Use 'showSqlForPostgres' instead" #-}
showSqlForPostgreSQLSimple' :: Unpackspec a -> Query a -> String
showSqlForPostgreSQLSimple' = showSqlForPostgres

{-# DEPRECATED showSqlForPostgreSQLSimple
    "Use 'showSqlForPostgres' instead" #-}
showSqlForPostgreSQLSimple :: Unpackspec a -> Query a -> String
showSqlForPostgreSQLSimple = showSqlForPostgres

-- Currently we only support SQL generation for Postgres because,
-- for example, 'cat' is implemented as '||' and the hackery we do
-- in, for example, Values.hs, may be Postgres specific.
--
-- Support for other DBMSes can be added if required.
showSqlForPostgres :: Unpackspec a -> Query a -> String
showSqlForPostgres = optimizeFormatAndShowSQL .: runQueryArrPrim

formatAndShowSQL :: PrimQuery -> String
formatAndShowSQL = show . ppSql . sqlQuery defaultSqlGenerator

optimizeFormatAndShowSQL :: PrimQuery -> String
optimizeFormatAndShowSQL = formatAndShowSQL . optimize

-- I don't really know why this is an orphan instance.
instance Default (PPOfContravariant Unpackspec) (Wire a) (Wire a) where
  def = PPOfContravariant (Unpackspec writerWire)

showSqlForPostgresDefault :: forall a.
                             Default (PPOfContravariant Unpackspec) a a
                             => Query a
                             -> String
showSqlForPostgresDefault = showSqlForPostgres def'
  where PPOfContravariant def' = def :: PPOfContravariant Unpackspec a a
