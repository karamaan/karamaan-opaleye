module Karamaan.Opaleye.Use where

import Karamaan.Opaleye.QueryArr (QueryArr)
import Karamaan.Opaleye.Wire (Wire)
import Karamaan.Opaleye.Operators2 (eq)
import Karamaan.Opaleye.Predicates (restrict)
import Control.Category ((<<<))
import Control.Arrow ((&&&), arr)

-- 'use x' restricts to the case where x = x, i.e. it is a null operation!
-- The only reason it exists is to slightly confuse the HaskellDB optimizer
-- so that it doesn't perform invalid transformations.
use :: QueryArr (Wire a) ()
use = restrict <<< eq <<< (arr id &&& arr id)
