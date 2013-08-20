{-# LANGUAGE Arrows #-}

module Karamaan.Opaleye.Bend where

import Karamaan.Opaleye.QueryArr (QueryArr)
import Control.Arrow ((<<<), arr, returnA)
import Karamaan.Opaleye.Wire (Wire)
import Karamaan.Opaleye.Operators2 (eq)
import Karamaan.Opaleye.Predicates (restrict)

bend :: (b -> Wire x) -> QueryArr a b -> QueryArr (Wire x, a) b
bend f q = proc (x, rest) -> do
  b <- q -< rest
  x' <- arr f -< b
  restrict <<< eq -< (x, x')
  returnA -< b
