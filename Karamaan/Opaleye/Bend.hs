{-# LANGUAGE Arrows #-}

module Karamaan.Opaleye.Bend where

import Karamaan.Opaleye.QueryArr (Query, QueryArr)
import Control.Arrow ((<<<), arr, returnA)
import Karamaan.Opaleye.Wire (Wire)
import Karamaan.Opaleye.Operators2 (eq)
import Karamaan.Opaleye.Predicates (restrict)
import qualified Karamaan.WhaleUtil.Arrow as UA

bend :: (b -> Wire x) -> QueryArr a b -> QueryArr (Wire x, a) b
bend f q = proc (x, rest) -> do
  b <- q -< rest
  x' <- arr f -< b
  restrict <<< eq -< (x, x')
  returnA -< b

bendFst :: Query (Wire a, Wire b) -> QueryArr (Wire a) (Wire b)
bendFst q = arr snd <<< bend fst q <<< UA.removeUnit

bendSnd :: Query (Wire a, Wire b) -> QueryArr (Wire b) (Wire a)
bendSnd q = arr fst <<< bend snd q <<< UA.removeUnit
