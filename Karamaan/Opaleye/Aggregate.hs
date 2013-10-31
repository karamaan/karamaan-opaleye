module Karamaan.Opaleye.Aggregate where

import Prelude hiding (sum)
import Karamaan.Opaleye.QueryArr (Query, runSimpleQueryArr,
                                  Tag, next, tagWith,
                                  simpleQueryArr)
import Database.HaskellDB.PrimQuery (PrimQuery(Project),
                                     AggrOp(AggrSum, AggrAvg, AggrMax,
                                            AggrCount),
                                     PrimExpr(AttrExpr,
                                              AggrExpr),
                                     Attribute)
import Karamaan.Opaleye.Wire (Wire)
import Karamaan.Opaleye.Colspec (Writer, PackMap, writerWire, packMapWire,
                                 runWriter, runPackMap, LWriter, writer)
import Data.Profunctor (Profunctor, dimap, lmap)
import Data.Profunctor.Product (ProductProfunctor, empty, (***!),
                                ProductContravariant, point, (***<),
                                p3)
import Data.Functor.Contravariant (contramap)

-- Maybe it would be safer if we combined the two writers into
-- "LWriter (Maybe AggrOp, String) a"?  That way we'd know they output
-- the same number of results
data Aggregator a b = Aggregator (LWriter (Maybe AggrOp) a) (Writer a)
                                 (PackMap a b)


instance Profunctor Aggregator where
  dimap f g (Aggregator a w p) = Aggregator (contramap f a) (contramap f w)
                                            (dimap f g p)

instance ProductProfunctor Aggregator where
  empty = Aggregator point point empty
  Aggregator a w p ***! Aggregator a' w' p' = Aggregator (a ***< a') (w ***< w')
                                                         (p ***! p')

aggregatorMaker :: AggrOp -> Aggregator (Wire a) (Wire b)
aggregatorMaker = aggregatorMaker' . Just

aggregatorMaker' :: Maybe AggrOp -> Aggregator (Wire a) (Wire b)
aggregatorMaker' op = Aggregator (writer (const [op])) writerWire packMapWire

-- TODO: the numeric ones should have some num constraint
sum :: Aggregator (Wire a) (Wire a)
sum = aggregatorMaker AggrSum

avg :: Aggregator (Wire a) (Wire a)
avg = aggregatorMaker AggrAvg

max :: Aggregator (Wire a) (Wire a)
max = aggregatorMaker AggrMax

groupBy :: Aggregator (Wire a) (Wire a)
groupBy = aggregatorMaker' Nothing

count :: Aggregator (Wire a) (Wire Int)
count = aggregatorMaker AggrCount

aggregate :: Aggregator a b -> Query a -> Query b
aggregate mf q = simpleQueryArr (aggregate'' mf . runSimpleQueryArr q)

-- This is messy and there should be a lot more structure to it, but I can't see
-- how, currently.  Once there's another function like this
-- and binrel it will perhaps be easy to see where the real duplication is.
aggregate'' :: Aggregator a b -> (a, PrimQuery, Tag) -> (b, PrimQuery, Tag)
aggregate'' mf (out, primQ', j) =
    let tag' :: String -> String
        tag' = tagWith j
        (Aggregator aggrs writer' mapper) = mf
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
