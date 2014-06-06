module Karamaan.Opaleye.HaskellDB where

import qualified Database.HaskellDB.Sql.Print as P
import qualified Database.HaskellDB.Sql as S
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ ((<+>), ($$))

-- HaskellDB.hs: Stuff that is arguably missing from HaskellDB

data SqlInsertReturning = SqlInsertReturning S.SqlInsert [S.SqlExpr]

ppInsertReturning :: SqlInsertReturning -> PP.Doc
ppInsertReturning (SqlInsertReturning insert returnExprs) =
  P.ppInsert insert
  $$ PP.text "RETURNING"
  <+> commaV P.ppSqlExpr returnExprs

-- TODO: this is from HaskellDB.Sql.Print, but they don't export it!
-- Why oh why?
commaV :: (a -> PP.Doc) -> [a] -> PP.Doc
commaV f = PP.vcat . PP.punctuate PP.comma . map f
