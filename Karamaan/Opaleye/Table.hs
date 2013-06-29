module Karamaan.Opaleye.Table where

import Karamaan.Opaleye.QueryArr (Query, QueryArr(QueryArr), next, tagWith)
import Database.HaskellDB.PrimQuery (PrimQuery(Project, BaseTable),
                                     PrimExpr(AttrExpr),
                                     Attribute, Assoc, times)
import Control.Arrow ((***))
import Karamaan.Opaleye.Pack (Pack, pack)
import Karamaan.Opaleye.Wire (Wire, unWire)
import Karamaan.Opaleye.Aggregate (Writer)
-- FIXME: don't want to import everything, but we're importing a lot
-- and I can't be bothered to type it all
import Karamaan.Opaleye.Aggregators hiding (chain)

data Colspec a = Colspec a (Writer a)

col :: Wire a -> Colspec (Wire a)
col w = Colspec w (return . unWire)

colsT1 :: T1 (Colspec a1) -> Colspec (T1 a1)
colsT1 = id

-- TODO: dup with *:
colsT2 :: T2 (Colspec a1) (Colspec a2) -> Colspec (T2 a1 a2)
colsT2 (Colspec a1 w1, Colspec a2 w2)
  = Colspec (a1, a2) w'
  where w' (x1, x2) = w1 x1 ++ w2 x2

chain :: (t -> Colspec b) -> (Colspec a, t) -> Colspec (a, b)
chain rest (a, as) = colsT2 (a, rest as)

colsT3 :: T3 (Colspec a1) (Colspec a2) (Colspec a3) -> Colspec (T3 a1 a2 a3)
colsT3 = chain colsT2

colsT4 :: T4 (Colspec a1) (Colspec a2) (Colspec a3) (Colspec a4)
          -> Colspec (T4 a1 a2 a3 a4)
colsT4 = chain colsT3

colsT5 :: T5 (Colspec a1) (Colspec a2) (Colspec a3) (Colspec a4) (Colspec a5)
          -> Colspec (T5 a1 a2 a3 a4 a5)
colsT5 = chain colsT4

colsT6 :: T6 (Colspec a1) (Colspec a2) (Colspec a3) (Colspec a4) (Colspec a5)
          (Colspec a6)
          -> Colspec (T6 a1 a2 a3 a4 a5 a6)
colsT6 = chain colsT5

colsT7 :: T7 (Colspec a1) (Colspec a2) (Colspec a3) (Colspec a4) (Colspec a5)
          (Colspec a6) (Colspec a7)
          -> Colspec (T7 a1 a2 a3 a4 a5 a6 a7)
colsT7 = chain colsT6

colsT8 :: T8 (Colspec a1) (Colspec a2) (Colspec a3) (Colspec a4) (Colspec a5)
          (Colspec a6) (Colspec a7) (Colspec a8)
          -> Colspec (T8 a1 a2 a3 a4 a5 a6 a7 a8)
colsT8 = chain colsT7

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
