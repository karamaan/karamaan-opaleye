module Karamaan.Opaleye.Nullable where

import Karamaan.Opaleye.QueryArr (QueryArr, Query)
import Karamaan.Opaleye.Wire (Wire)
import qualified Karamaan.Opaleye.Operators2 as Op2
import qualified Karamaan.Opaleye.Join as Join
import Control.Arrow ((<<<), second)

-- This is just used a phantom type in 'Wire's.
-- It's not actually used for values.
--data Nullable a = PhantomNullable

-- For now just use a type synonym.  We will switch to a type later.
type Nullable = Maybe

-- In the ideal world we are creating the 'Maybe' functions in Op2 go
-- away and these are implemented directly.
isNull :: QueryArr (Wire (Nullable a)) (Wire Bool)
isNull = Join.unsafeCoerce <<< Op2.isNull <<< Join.unsafeCoerce

fromNullable :: QueryArr (Wire a, Wire (Maybe a)) (Wire a)
fromNullable = Op2.fromMaybe <<< second Join.unsafeCoerce

fromNullable' :: Query (Wire a) -> QueryArr (Wire (Nullable a)) (Wire a)
fromNullable' w = Op2.fromMaybe' w <<< Join.unsafeCoerce
