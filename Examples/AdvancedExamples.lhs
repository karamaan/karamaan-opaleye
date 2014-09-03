> {-# LANGUAGE Arrows #-}
>
> module AdvancedExamples where
>
> import Prelude hiding (sum, min, max)
> import Karamaan.Opaleye.Table (makeTableDef)
> import Karamaan.Opaleye.QueryArr (Query)
> import Karamaan.Opaleye.Aggregate (aggregate, stddev, min, max)
> import qualified Karamaan.Opaleye.AggregateSuper as A
> import qualified Karamaan.Opaleye.Operators.Numeric as N
> import qualified Karamaan.Opaleye.ExprArr as E
> import Karamaan.Opaleye.Wire (Wire)
> import Control.Arrow ((<<<), arr, Arrow)
>
> import Data.Profunctor.Product (p3)
> import Examples (sh)
> import Data.Int (Int64)

> table :: Query (Wire Double)
> table = makeTableDef "myColumn" "myTable"

> triple :: Arrow arr => arr a (a, a, a)
> triple = arr (\a -> (a, a, a))

> cols3 :: Query (Wire Double, Wire Double, Wire Double)
> cols3 = triple <<< table

> statistics :: Query (Wire Double, Wire Double, Wire Double)
> statistics = aggregate (p3 (min, max, stddev)) cols3

> spread :: Query (Wire Double)
> spread = proc () -> do
>   (cMin, cMax, cStdDev) <- statistics -< ()
>   range <- N.minus -< (cMax, cMin)
>   N.divide -< (range, cStdDev)

> spreadAgg :: A.Aggregator (Wire Double) (Wire Double)
> spreadAgg = a
>   where a = (A.rmapExpr numericalExpr
>              . A.lmapExpr triple) (A.old (p3 (min, max, stddev)))
>         numericalExpr = proc (cMin, cMax, cStdDev) -> do
>                           range <- E.minus -< (cMax, cMin)
>                           E.divide -< (range, cStdDev)

ghci> sh spread
-- FIXME: this one has a bug because of the repeated column names!

> runSpreadAgg :: Query (Wire Double)
> runSpreadAgg = A.aggregate spreadAgg table

ghci> sh runSpreadAgg 
-- FIXME: this one has a bug because of the repeated column names!

With new aggregators we can use 'count' without having to feed in a
column.

> count :: Query (Wire Int64)
> count = A.aggregate A.countAll (arr (const ()) <<< table)

ghci> sh count
SELECT COUNT(0) as constant_2_3
FROM myTable as T1
