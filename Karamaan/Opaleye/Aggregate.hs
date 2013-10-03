module Karamaan.Opaleye.Aggregate where

import Karamaan.Opaleye.QueryArr (Query, QueryArr(QueryArr), runQueryArr,
                                  Tag, next, tagWith)
import Database.HaskellDB.PrimQuery (PrimQuery(Project, Empty),
                                     AggrOp(AggrSum, AggrAvg, AggrMax,
                                            AggrCount),
                                     PrimExpr(AttrExpr,
                                              AggrExpr),
                                     Attribute, times)
import Karamaan.Opaleye.Wire (Wire)
import Control.Arrow ((&&&))
import Karamaan.Opaleye.Colspec (Writer, PackMap, writerWire, packMapWire,
                                 runWriter, runPackMap)
import Karamaan.Opaleye.ProductProfunctor ((***!), (***<))

-- I used to have "Aggregator a b" for a's that would get turned
-- into b's when aggregated, but I never used it.
-- I removed it for simplicity, but it might need to be
-- used again at some point in the future
data Aggregator a = Aggregator [Maybe AggrOp] (Writer a) (PackMap a a)

instance Show (Aggregator a) where
  show (Aggregator ops _ _) = show ops

(*:) :: Aggregator a -> Aggregator a' -> Aggregator (a, a')
(Aggregator s w p) *: (Aggregator s' w' p') = Aggregator s'' w'' p''
  where s'' = s ++ s'
        w'' = w ***< w'
        p'' = p ***! p'

aggregatorMaker :: AggrOp -> Aggregator (Wire a)
aggregatorMaker = aggregatorMaker' . Just

aggregatorMaker' :: Maybe AggrOp -> Aggregator (Wire a)
aggregatorMaker' op = Aggregator [op] writerWire packMapWire

sum :: Aggregator (Wire a)
sum = aggregatorMaker AggrSum

avg :: Aggregator (Wire a)
avg = aggregatorMaker AggrAvg

max :: Aggregator (Wire a)
max = aggregatorMaker AggrMax

groupBy :: Aggregator (Wire a)
groupBy = aggregatorMaker' Nothing

count :: Aggregator (Wire a)
count = aggregatorMaker AggrCount

aggregate :: Aggregator a -> Query a -> Query a
aggregate mf q = QueryArr (\((), primQuery, t0) ->
  let (a, primQ, t1) = runQueryArr q ((), Empty, t0)
      (the_new_names, t2, primQ') = aggregate'' mf a t1 primQ
  in (the_new_names, times primQuery primQ', t2))

-- This is messy and there should be a lot more structure to it, but I can't see
-- how, currently.  Once there's another function like this
-- and binrel it will perhaps be easy to see where the real duplication is.
aggregate'' :: Aggregator t -> t -> Tag -> PrimQuery -> (t, Tag, PrimQuery)
aggregate'' mf out j primQ' =
    let tag' :: String -> String
        tag' = tagWith j
        aggrs :: [Maybe AggrOp]
        (Aggregator aggrs writer' mapper) = mf
        old_names :: [String]
        old_names = runWriter writer' out
        new_names :: [String]
        new_names = map tag' old_names
        zipped :: [(String, Maybe AggrOp, String)]
        zipped = zip3 new_names aggrs old_names
        jobber :: PrimQuery
        jobber = Project (map assoc zipped) primQ'
    in (runPackMap mapper tag' out, next j, jobber)

assoc :: (String, Maybe AggrOp, String) -> (Attribute, PrimExpr)
assoc (snew, mop, sold) = (snew, makeAggr mop (AttrExpr sold))

makeAggr :: Maybe AggrOp -> PrimExpr -> PrimExpr
makeAggr = maybe id AggrExpr

-- Christopher preferred this API for aggregation
(+:) :: (Aggregator a, x -> a) -> (Aggregator b, x -> b)
        -> (Aggregator (a, b), x -> (a, b))
(agg, f) +: (agg', f') = (agg *: agg', f &&& f')

aggregate' :: (Aggregator a, x -> a) -> Query x -> Query a
aggregate' (m, f) q = aggregate m (fmap f q)

example :: Query (Wire String, Wire Int)
           -> Query ((Wire String, Wire Int), Wire Int)
example = aggregate' $ (groupBy, fst)
                       +: (Karamaan.Opaleye.Aggregate.sum, snd)
                       +: (avg, snd)
