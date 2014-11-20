{-# LANGUAGE Arrows #-}

module Karamaan.Opaleye.Nullable where

import Karamaan.Opaleye.QueryArr (QueryArr, Query)
import Karamaan.Opaleye.Wire (Wire)
import qualified Karamaan.Opaleye.Wire as Wire
import qualified Karamaan.Opaleye.ExprArr as E
import qualified Karamaan.Opaleye.Operators2 as Op2
import Control.Arrow (arr, (<<<))
import Database.HaskellDB.PrimQuery (UnOp(OpIsNull,OpIsNotNull))
import qualified Database.HaskellDB.PrimQuery as PQ

-- TODO: At the appropriate time we will replace the Nullable type
-- synonym to Maybe with its own type, and then the transition to
-- Nullable will be complete!

-- This is just used a phantom type in 'Wire's.
-- It's not actually used for values.
--data Nullable a = PhantomNullable

-- FIXME: Shouldn't these all now deal with ExprArr rather than QueryArr?

-- For now just use a type synonym.  We will switch to a type later.
-- Don't use Maybe in Wires in new code!
type Nullable = Maybe

-- TODO: perhaps this belongs elsewhere, but we need to work out how to avoid
-- circular dependencies
unsafeCoerce :: QueryArr (Wire a) (Wire b)
unsafeCoerce = arr Wire.unsafeCoerce

isNullExpr :: E.ExprArr (Wire (Nullable a)) (Wire Bool)
isNullExpr = E.unOp OpIsNull "is_null"

isNull :: QueryArr (Wire (Nullable a)) (Wire Bool)
isNull = E.toQueryArrDef isNullExpr

isNotNullExpr :: E.ExprArr (Wire (Nullable a)) (Wire Bool)
isNotNullExpr = E.unOp OpIsNotNull "is_not_null"

isNotNull :: QueryArr (Wire (Nullable a)) (Wire Bool)
isNotNull = E.toQueryArrDef isNotNullExpr

fromNullable :: QueryArr (Wire a, Wire (Maybe a)) (Wire a)
fromNullable = proc (d, m) -> do
  isNull' <- isNull -< m
  Op2.ifThenElse -< (isNull', d, Wire.unsafeCoerce m)

fromNullable' :: Query (Wire a) -> QueryArr (Wire (Nullable a)) (Wire a)
fromNullable' d = proc m -> do
  d' <- d -< ()
  fromNullable -< (d', m)

toNullable :: QueryArr (Wire a) (Wire (Nullable a))
toNullable = unsafeCoerce

toNullableExpr :: E.ExprArr (Wire a) (Wire (Nullable a))
toNullableExpr = E.unsafeCoerce

null :: E.Expr (Wire (Nullable a))
null = E.unsafeCoerce <<< E.constantLit PQ.NullLit

joinNullable :: E.ExprArr (Wire (Nullable (Nullable a))) (Wire (Nullable a))
joinNullable = arr Wire.unsafeCoerce
