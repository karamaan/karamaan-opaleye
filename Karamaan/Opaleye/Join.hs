{-# LANGUAGE Arrows, FlexibleContexts #-}

module Karamaan.Opaleye.Join (semijoin) where

import Karamaan.Opaleye.QueryArr (Query, QueryArr)
import Karamaan.Opaleye.Wire (Wire)
import qualified Karamaan.Opaleye.Wire as Wire
import Karamaan.Opaleye.Default (Default)
import Karamaan.Opaleye.Colspec (Colspec')
import Karamaan.Opaleye.Predicates (restrict)
import Karamaan.Opaleye.Operators2 (union, isNull, eq)
import Control.Arrow (returnA, arr, first)
import Control.Category ((<<<))

-- FIXME: duplication with Report.Position.Coercions.unsafeCoerce
unsafeCoerce :: QueryArr (Wire a) (Wire b)
unsafeCoerce = arr Wire.unsafeCoerce

-- This is slightly subtle.  The eq *can* return Unknown
-- (see http://en.wikipedia.org/wiki/Null_%28SQL%29#Comparisons_with_NULL_and_the_three-valued_logic_.283VL.29)
-- but since we restrict immediately the Unknown is indistinguishable
-- from false.
restrictMaybeEq :: QueryArr (Wire (Maybe a), Wire a) ()
restrictMaybeEq = restrict <<< eq <<< first unsafeCoerce

joinWithoutNull :: Query (a, Wire (Maybe b))
                -> Query (Wire b, c)
                -> Query (a, c)
joinWithoutNull q1 q2 = proc () -> do
  (a, mb) <- q1 -< ()
  (b, c) <- q2 -< ()

  restrictMaybeEq -< (mb, b)

  returnA -< (a, c)

joinWithNull :: Query (a, Wire (Maybe b))
             -> Query c
             -> Query (a, c)
joinWithNull q1 q2 = proc () -> do
  (a, mb) <- q1 -< ()
  restrict <<< isNull -< mb
  replacement <- q2 -< ()
  returnA -< (a, replacement)

-- FIXME: comment this
semijoin :: (Default Colspec' a a, Default Colspec' c c)
         => Query (a, Wire (Maybe b))
         -> Query (Wire b, c)
         -> Query c
         -> Query (a, c)
semijoin q1 q2 q3 = joinWithoutNull q1 q2 `union` joinWithNull q1 q3
