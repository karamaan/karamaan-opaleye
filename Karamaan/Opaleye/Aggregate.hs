-- | Perform aggregations on query results.
module Karamaan.Opaleye.Aggregate
    ( -- * Working with 'Aggregator's
      Aggregator
    , aggregate

      -- * Standard Aggregations
    , avg
    , count
    , groupBy
    , max
    , min
    , nullableMax
    , sum
    )
    where

import Prelude hiding (max, sum, min)
import Karamaan.Opaleye.Nullable (Nullable)
import Karamaan.Opaleye.QueryArr (Query, runSimpleQueryArr,
                                  Tag, next, tagWith,
                                  simpleQueryArr)
import Database.HaskellDB.PrimQuery (PrimQuery(Project),
                                     AggrOp(AggrSum, AggrAvg, AggrMax, AggrMin,
                                            AggrCount),
                                     PrimExpr(AttrExpr,
                                              AggrExpr),
                                     Attribute)
import Karamaan.Opaleye.Wire (Wire)
import Karamaan.Opaleye.QueryColspec (Writer, PackMap, writerWire, packMapWire,
                                      runWriter, runPackMap, LWriter, writer)
import Data.Profunctor (Profunctor, dimap)
import Data.Profunctor.Product (ProductProfunctor, empty, (***!),
                                defaultEmpty, defaultProfunctorProduct)
import Data.Functor.Contravariant (contramap)
import Control.Applicative (Applicative, (<*>), pure)
import Data.Monoid (mempty, (<>))

-- Maybe it would be safer if we combined the two writers into
-- "LWriter (Maybe AggrOp, String) a"?  That way we'd know they output
-- the same number of results
{-|

An 'Aggregator' is a 'Profunctor' that takes (potentially many) rows of type
@a@, groups them, and transforms each group into a single row of type @b@. This
corresponds to aggregators using @GROUP BY@ in SQL.

-}
data Aggregator a b = Aggregator (LWriter (Maybe AggrOp) a) (Writer a)
                                 (PackMap a b)

{-|

Given a 'Query' producing rows of type @a@ and an 'Aggregator' accepting rows of
type @a@, apply the aggregator to the results of the query.

-}
aggregate :: Aggregator a b -> Query a -> Query b
aggregate agg q = simpleQueryArr (aggregate' agg . runSimpleQueryArr q)

instance Functor (Aggregator a) where
  fmap f (Aggregator a w p) = Aggregator a w (fmap f p)

instance Applicative (Aggregator a) where
  pure = Aggregator mempty mempty . pure
  Aggregator a w p <*> Aggregator a' w' p' =
    Aggregator (a <> a') (w <> w') (p <*> p')

instance Profunctor Aggregator where
  dimap f g (Aggregator a w p) =
    Aggregator (contramap f a) (contramap f w) (dimap f g p)

instance ProductProfunctor Aggregator where
  empty = defaultEmpty
  (***!) = defaultProfunctorProduct

aggregatorMaker :: AggrOp -> Aggregator (Wire a) (Wire b)
aggregatorMaker = aggregatorMaker' . Just

aggregatorMaker' :: Maybe AggrOp -> Aggregator (Wire a) (Wire b)
aggregatorMaker' op = Aggregator (writer (const [op])) writerWire packMapWire

-- TODO: the numeric ones should have some num constraint
-- TODO: actually Postgres returns NULL for the sum of an empty column!
--       We should make sure to postcompose with a fromNullable to set it to
--       zero

-- | Sum all rows in a group.
sum :: Aggregator (Wire a) (Wire a)
sum = aggregatorMaker AggrSum

-- | Aggregate each group to the average value in that group.
avg :: Aggregator (Wire a) (Wire a)
avg = aggregatorMaker AggrAvg

-- | Aggregate each group to the maximum value in that group.
max :: Aggregator (Wire a) (Wire a)
max = aggregatorMaker AggrMax

-- | Aggregate each group to the maximum value in that group. If the group is empty , the result is @null@.
nullableMax :: Aggregator (Wire a) (Wire (Nullable a))
nullableMax = aggregatorMaker AggrMax

-- | Aggregate each group to the minimum value in that group.
min :: Aggregator (Wire a) (Wire a)
min = aggregatorMaker AggrMin

-- | Group the aggregation by equality on the input to 'groupBy'.
groupBy :: Aggregator (Wire a) (Wire a)
groupBy = aggregatorMaker' Nothing

-- | Count the amount of rows inside each @GROUP BY@ clause (or the entire table
-- if no grouping was specified).
count :: Aggregator (Wire a) (Wire Int)
count = aggregatorMaker AggrCount

-- This is messy and there should be a lot more structure to it, but I can't see
-- how, currently.  Once there's another function like this
-- and binrel it will perhaps be easy to see where the real duplication is.
aggregate' :: Aggregator a b -> (a, PrimQuery, Tag) -> (b, PrimQuery, Tag)
aggregate' agg (out, primQ', j) =
    let tag' :: String -> String
        tag' = tagWith j
        (Aggregator aggrs writer' mapper) = agg
        old_names :: [String]
        old_names = runWriter writer' out
        new_names :: [String]
        new_names = map tag' old_names
        zipped :: [(String, Maybe AggrOp, String)]
        zipped = zip3 new_names (runWriter aggrs out) old_names
        jobber :: PrimQuery
        jobber = Project (map assoc zipped) primQ'
    in (runPackMap mapper tag' out, jobber, next j)

assoc :: (String, Maybe AggrOp, String) -> (Attribute, PrimExpr)
assoc (snew, mop, sold) = (snew, makeAggr mop (AttrExpr sold))

makeAggr :: Maybe AggrOp -> PrimExpr -> PrimExpr
makeAggr = maybe id AggrExpr
