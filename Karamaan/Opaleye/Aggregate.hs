module Karamaan.Opaleye.Aggregate
    ( count
    , avg
    , sum
    , groupBy
    , aggregate
    , (>:)
    , max
    , min
    , nullableMax
    , Aggregator
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
import Data.Profunctor (Profunctor, dimap, lmap)
import Data.Profunctor.Product (ProductProfunctor, empty, (***!),
                                defaultEmpty, defaultProfunctorProduct,
                                p3)
import Data.Functor.Contravariant (contramap)
import Control.Applicative (Applicative, (<*>), pure)
import Data.Monoid (Monoid, mempty, (<>))

-- Maybe it would be safer if we combined the two writers into
-- "LWriter (Maybe AggrOp, String) a"?  That way we'd know they output
-- the same number of results
data Aggregator a b = Aggregator (LWriter (Maybe AggrOp) a) (Writer a)
                                 (PackMap a b)


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
sum :: Aggregator (Wire a) (Wire a)
sum = aggregatorMaker AggrSum

avg :: Aggregator (Wire a) (Wire a)
avg = aggregatorMaker AggrAvg

max :: Aggregator (Wire a) (Wire a)
max = aggregatorMaker AggrMax

nullableMax :: Aggregator (Wire a) (Wire (Nullable a))
nullableMax = aggregatorMaker AggrMax

min :: Aggregator (Wire a) (Wire a)
min = aggregatorMaker AggrMin

groupBy :: Aggregator (Wire a) (Wire a)
groupBy = aggregatorMaker' Nothing

count :: Aggregator (Wire a) (Wire Int)
count = aggregatorMaker AggrCount

aggregate :: Aggregator a b -> Query a -> Query b
aggregate agg q = simpleQueryArr (aggregate' agg . runSimpleQueryArr q)

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

-- Christopher preferred this API for aggregation
(>:) :: Aggregator a b -> (x -> a) -> Aggregator x b
(>:) = flip lmap

pa3 :: ProductProfunctor p => (p a b1, p a b2, p a b3) -> p a (b1, b2, b3)
pa3 = lmap (\x -> (x,x,x)) . p3

example :: Aggregator (Wire String, Wire Int)
                      (Wire String, Wire Int, Wire Int)
example = pa3 (groupBy >: fst, sum >: snd, avg >: snd)
