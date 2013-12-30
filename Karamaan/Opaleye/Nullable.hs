module Karamaan.Opaleye.Nullable where

import Karamaan.Opaleye.QueryArr (QueryArr)
import Karamaan.Opaleye.Wire (Wire)
import qualified Karamaan.Opaleye.Operators2 as Op2
import qualified Karamaan.Opaleye.Join as Join
import Control.Arrow ((<<<))

-- This is just used a phantom type in 'Wire's.
-- It's not actually used for values.
data Nullable a = PhantomNullable

-- In the ideal world we are creating, Op2.isNull goes away because it
-- uses the old Maybe type constructor, and this is implemented
-- directly.
isNull :: QueryArr (Wire (Nullable a)) (Wire Bool)
isNull = Join.unsafeCoerce <<< Op2.isNull <<< Join.unsafeCoerce
