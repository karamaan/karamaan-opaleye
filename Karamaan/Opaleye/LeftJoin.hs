{-# LANGUAGE Arrows, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses #-}

module Karamaan.Opaleye.LeftJoin where

import Karamaan.Opaleye.Default (Default, def)
import Karamaan.Opaleye.Colspec (Colspec')
import Karamaan.Opaleye.QueryArr (Query)
import Karamaan.Opaleye.Wire (Wire)
import qualified Karamaan.Opaleye.Wire as Wire
import qualified Karamaan.Opaleye.Operators2 as Op2
import Control.Arrow (arr, returnA, (<<<), (***), (&&&))
import qualified Karamaan.Opaleye.Predicates as P
import Data.Profunctor (Profunctor, dimap)
import Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Database.HaskellDB.PrimQuery as PQ

-- FIXME: this seems to fail on the union because the NULLs are not
-- given explicit types.  This is an annoyance of Postgres.  There'll be
-- a way to work around it (of course: just give the NULLs explicity types!)
-- but it is annoying.

-- I don't really like that this is 'a -> b'.  To be safe it should be
-- QueryArr a b, or ExprArr a b, when that exists.  I don't think it
-- will cause any problems though, if it is not exported.
data NullMaker a b = NullMaker (a -> b) (Query b)

toNullable :: NullMaker a b -> a -> b
toNullable (NullMaker f _) = f

-- When we have proper support for ExprArr I suppose this can be
-- NullMaker a b -> Expr b
nulls :: NullMaker a b -> Query b
nulls (NullMaker _ n) = n

instance Default NullMaker (Wire a) (Wire (Maybe a)) where
  def = NullMaker Wire.unsafeCoerce (Op2.constantLit PQ.NullLit)

instance Profunctor NullMaker where
  dimap f g nm = NullMaker (g . toNullable nm . f) (arr g <<< nulls nm)

instance ProductProfunctor NullMaker where
  empty = NullMaker id (arr id)
  NullMaker f n ***! NullMaker f' n' = NullMaker (f *** f') (n &&& n')

leftJoin :: (Default Colspec' l l, Default Colspec' r' r')
         => NullMaker r r'
         -> Query l -> (l -> Wire b)
         -> Query r -> (r -> Wire b)
         -> Query (l, r')
leftJoin nm qL fL qR fR = join `Op2.union` outer
  where join = proc () -> do
          rowL <- qL -< ()
          keyL <- arr fL -< rowL

          rowR <- qR -< ()
          keyR <- arr fR -< rowR

          P.restrict <<< Op2.eq -< (keyL, keyR)

          returnA -< (rowL, toNullable nm rowR)

        outer = proc () -> do
          rowL <- qL `Op2.difference` (arr fst <<< join) -< ()

          nulls' <- nulls nm -< ()

          returnA -< (rowL, nulls')
