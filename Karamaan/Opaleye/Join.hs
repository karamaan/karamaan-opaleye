{-# LANGUAGE Arrows, FlexibleContexts #-}

module Karamaan.Opaleye.Join (semijoin) where

import Karamaan.Opaleye.QueryArr (Query, QueryArr)
import Karamaan.Opaleye.Wire (Wire)
import Data.Profunctor.Product.Default (Default)
import Karamaan.Opaleye.QueryColspec (QueryColspec)
import Karamaan.Opaleye.Predicates (restrict)
import Karamaan.Opaleye.Operators2 (union, eq)
import qualified Karamaan.Opaleye.Nullable as Nullable
import Control.Arrow (returnA, first)
import Control.Category ((<<<))

{- Herein are contained some abstractions that can help with
simulating outer-join like behaviour in Opaleye.  The correct
long-term approach is to actually implement outer joins in Opaleye,
but this requires patching HaskellDB or developing our own AST
replacement so it is slightly harder than trivial.
-}

-- This is slightly subtle.  The eq *can* return Unknown
-- (see http://en.wikipedia.org/wiki/Null_%28SQL%29#Comparisons_with_NULL_and_the_three-valued_logic_.283VL.29)
-- but since we restrict immediately the Unknown is not distinguised
-- from false in the overall result.
restrictMaybeEq :: QueryArr (Wire (Maybe a), Wire a) ()
restrictMaybeEq = restrict <<< eq <<< first Nullable.unsafeCoerce

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
  restrict <<< Nullable.isNull -< mb
  replacement <- q2 -< ()
  returnA -< (a, replacement)


-- Semijoin effectively does a LEFT JOIN between queries.
--
-- q1 has a nullable column, and it is joined to q2 which has a non-nullable
-- column of the same type.  Where nulls appear in the column of q1 they are of
-- course not joined to q2.  Instead they are replaced by the value of q3.
--
-- Be careful with q3.  If it contains more than one row then you will get
-- duplicated rows in the output.
--
-- I suspect this could actually be implemented in terms of a LEFT JOIN, and
-- that might even give better performance.
semijoin :: (Default QueryColspec a a, Default QueryColspec c c)
         => Query (a, Wire (Maybe b))
         -> Query (Wire b, c)
         -> Query c
         -> Query (a, c)
semijoin q1 q2 q3 = joinWithoutNull q1 q2 `union` joinWithNull q1 q3
