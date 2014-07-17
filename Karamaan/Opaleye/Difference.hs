{-# LANGUAGE Arrows #-}

module Karamaan.Opaleye.Difference where

import Karamaan.Opaleye.QueryArr (Query)
import Karamaan.Opaleye.Wire (Wire)
import qualified Karamaan.Opaleye.Distinct as D
import qualified Karamaan.Opaleye.Operators2 as O
import qualified Karamaan.Opaleye.Predicates as P
import Control.Arrow ((<<<), returnA, arr)

-- 'without column exclude q' returns all the rows of 'q' except those
-- where the column of 'q' selected by 'column' occurs in 'exclude'
without :: (b -> Wire a) -> Query (Wire a) -> Query b -> Query b
without f exclude q = proc () -> do
  keep <- D.distinctBetter (arr f <<< q) `O.difference` exclude -< ()
  row  <- q -< ()
  P.restrict <<< O.eq -< (keep, f row)
  returnA -< row
