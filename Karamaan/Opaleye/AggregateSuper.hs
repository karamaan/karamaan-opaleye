{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Karamaan.Opaleye.AggregateSuper where

import qualified Karamaan.Opaleye.Aggregate as A
import qualified Karamaan.Opaleye.ExprArr as E
import qualified Karamaan.Opaleye.Unpackspec as U
import qualified Karamaan.Opaleye.QueryArr as Q
import qualified Data.Profunctor.Product as PP
import Data.Profunctor.Product ((***!), (***<))
import qualified Data.Monoid as M
import qualified Data.Profunctor.Product.Default as D
import Data.Profunctor.Product (ProductProfunctor)
import Data.Profunctor (Profunctor, dimap)
import Karamaan.Opaleye.Wire (Wire)

import Control.Applicative (Applicative, (<*>), pure)
import Control.Arrow ((>>>), (<<<), arr, (***))
import qualified Control.Arrow as Arr
import qualified Control.Category as C
import Data.Int (Int64)

data Aggregator a b = forall a' b'.
                      Aggregator (E.ExprArr a a')
                                 (U.Unpackspec a')
                                 (A.Aggregator a' b')
                                 (U.Unpackspec b')
                                 (E.ExprArr b' b)

instance Functor (Aggregator a) where
  fmap f (Aggregator l ul a ur r) = Aggregator l ul a ur (fmap f r)

instance Applicative (Aggregator a) where
  pure x = Aggregator (Arr.arr (const ())) M.mempty PP.empty M.mempty
                      (Arr.arr (pure x))
  Aggregator fl ful fa fur fr <*> Aggregator xl xul xa xur xr = p
    where p = Aggregator ((fl *** xl) <<< arr (\x -> (x,x)))
                         (ful ***< xul) (fa ***! xa)
                         (fur ***< xur)
                         (arr (uncurry ($)) <<< (fr *** xr))

instance Profunctor Aggregator where
  dimap f g = lmapExpr (arr f) . rmapExpr (arr g)

instance ProductProfunctor Aggregator where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

lmapExpr :: E.ExprArr a' a -> Aggregator a b -> Aggregator a' b
lmapExpr f (Aggregator l ul a ur r) = Aggregator (f >>> l) ul a ur r

rmapExpr :: E.ExprArr b b' -> Aggregator a b -> Aggregator a b'
rmapExpr f (Aggregator l ul a ur r) = Aggregator l ul a ur (r >>> f)

aggregateExplicit :: U.Unpackspec a -> U.Unpackspec b
             -> Aggregator a b -> Q.Query a -> Q.Query b
aggregateExplicit ul1 ur2 (Aggregator l ul2 a ur1 r)  =
  (e2 <<<) . A.aggregate a . (e1 <<<)
  where e1 = E.toQueryArr ul1 ul2 l
        e2 = E.toQueryArr ur1 ur2 r

aggregate :: (D.Default (PP.PPOfContravariant U.Unpackspec) a a,
              D.Default (PP.PPOfContravariant U.Unpackspec) b b)
              => Aggregator a b -> Q.Query a -> Q.Query b
aggregate = aggregateExplicit D.cdef D.cdef

old :: (D.Default (PP.PPOfContravariant U.Unpackspec) a a,
        D.Default (PP.PPOfContravariant U.Unpackspec) b b) =>
       A.Aggregator a b -> Aggregator a b
old a = Aggregator C.id D.cdef a D.cdef C.id

countAll :: Aggregator () (Wire Int64)
countAll = lmapExpr (E.constant (0 :: Int)) (old A.count)
