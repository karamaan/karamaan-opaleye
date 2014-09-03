{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}
module Karamaan.Opaleye.Order
  ( -- * Ordering
    orderBy, orderByU
  , asc, desc, OrderSpec

    -- * Limiting
  , limit
  ) where

import Database.HaskellDB.PrimQuery ( PrimQuery (..), SpecialOp (..)
                                    , OrderExpr (..), PrimExpr
                                    , OrderOp (..)
                                    )
import Data.Functor.Contravariant (Contravariant (..))
import Data.Monoid (Monoid)
import Data.Profunctor.Product (PPOfContravariant)
import Data.Profunctor.Product.Default (Default, cdef)
import Karamaan.Opaleye.ExprArr (ExprArr, runExprArr'', Scope, scopeOfCols)
import Karamaan.Opaleye.QueryArr (Query, simpleQueryArr, runSimpleQueryArr, Tag)
import Karamaan.Opaleye.Unpackspec (Unpackspec)
import Karamaan.Opaleye.Wire (Wire)


-- * Ordering


{-|

Order the rows of a `Query` according to the specifications in
`OrderSpec`.

-}
orderBy :: Default (PPOfContravariant Unpackspec) a a => OrderSpec a -> Query a -> Query a
orderBy = orderByU cdef

{-|

Order the rows of a `Query` like `orderBy`, but with an explicit `Unpackspec`.

-}
orderByU :: Unpackspec a -> OrderSpec a -> Query a -> Query a
orderByU unpack oss a = simpleQueryArr (orderByU' unpack oss . runSimpleQueryArr a)

orderByU' :: Unpackspec a -> OrderSpec a -> (a, PrimQuery, Tag) -> (a, PrimQuery, Tag)
orderByU' unpack os (x, q, t) = (x, mkOrder, t)
  where
    mkOrder = Special (Order orderExprs) q
    orderExprs = toOrderExprs unpack x os

{- |

An `OrderSpec` represents an expression to order on and a sort
direction. Multiple ones can be composed with `Data.Monoid.mappend`.
If two rows are equal according to the first `OrderSpec`, the second
is used, and so on.

-}
newtype OrderSpec a = OrderSpec [SingleOrderSpec a] deriving Monoid

instance Contravariant OrderSpec where
  contramap f (OrderSpec oss) = OrderSpec $ map (contramap f) oss

data SingleOrderSpec a = SingleOrderSpec OrderOp (a -> Scope -> PrimExpr)

instance Contravariant SingleOrderSpec where
  contramap f (SingleOrderSpec op g) = SingleOrderSpec op (g . f)

toOrderExprs :: Unpackspec a -> a -> OrderSpec a -> [OrderExpr]
toOrderExprs unpack x (OrderSpec oss) = map (toOrderExpr unpack x) oss

toOrderExpr :: Unpackspec a -> a -> SingleOrderSpec a -> OrderExpr
toOrderExpr unpack x (SingleOrderSpec dir f) = OrderExpr dir (f x scope)
  where scope = scopeOfCols unpack x

{- |

Specify an ascending ordering by the given expression.

-}
asc :: ExprArr a (Wire b) -> OrderSpec a
asc  = orderSpec OpAsc

{- |

Specify an descending ordering by the given expression.

-}
desc :: ExprArr a (Wire b) -> OrderSpec a
desc = orderSpec OpDesc

orderSpec :: OrderOp -> ExprArr a (Wire b) -> OrderSpec a
orderSpec dir expr = OrderSpec [SingleOrderSpec dir (curry $ runExprArr'' expr)]


-- * Limiting


{- |

Limit the results of the given query to the given maximum number of
items.

-}

limit :: Int -> Query a -> Query a
limit n a = simpleQueryArr (limit' n . runSimpleQueryArr a)

limit' :: Int -> (a, PrimQuery, Tag) -> (a, PrimQuery, Tag)
limit' n (x, q, t) = (x, Special (Top n) q, t)
