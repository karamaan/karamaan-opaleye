> {-# LANGUAGE Arrows, FlexibleContexts #-}
> -- TODO: Get rid of FlexibleContexts if we ever move the definition of s
> --    elsewhere.
> {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
> {-# LANGUAGE TemplateHaskell #-}
>
> module AdvancedExamples where
>
> import Prelude hiding (sum, min, max)
> import Karamaan.Opaleye.Table (makeTableDef)
> import Karamaan.Opaleye.QueryArr (Query)
> import Karamaan.Opaleye.Aggregate (Aggregator, aggregate, stddev, min, max)
> import qualified Karamaan.Opaleye.Operators.Numeric as N
> import Karamaan.Opaleye.Wire (Wire)
> import Control.Arrow (returnA)
> import qualified Data.Profunctor as P
>
> import Data.Profunctor.Product (p3)
> import Examples (sh)

> table :: Query (Wire Double)
> table = makeTableDef "myColumn" "myTable"

> cols3 :: Query (Wire Double, Wire Double, Wire Double)
> cols3 = proc () -> do
>   col <- table -< ()
>   returnA -< (col, col, col)

> statistics :: Query (Wire Double, Wire Double, Wire Double)
> statistics = aggregate (p3 (min, max, stddev)) cols3

> foo = proc () -> do
>   (cMin, cMax, cStdDev) <- statistics -< ()
>   returnA -< (cMin, cMax, cStdDev)

> spread :: Query (Wire Double)
> spread = proc () -> do
>   (cMin, cMax, cStdDev) <- statistics -< ()
>   range <- N.minus -< (cMax, cMin)
>   N.divide -< (range, cStdDev)

> spreadAgg :: Aggregator (Wire Double) (Wire Double)

-- Oh dear we actually need expression operations to be pure for this
-- to work!  We can't do the calculation on the results side otherwise!

ghci> sh spread
