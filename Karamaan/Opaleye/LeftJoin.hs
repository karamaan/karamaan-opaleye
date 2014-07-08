{-# LANGUAGE Arrows, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses #-}

module Karamaan.Opaleye.LeftJoin where

{-
A hacky implementation of LEFT JOIN for Opaleye.

    leftJoin' q1 q2 expr

does the equivalent of

    q1 LEFT OUTER JOIN q2 ON expr

There is a better approach to implementing LEFT JOIN in Opaleye but it
will be more time consuming:

1. Implement an outer join primative in HaskellDB's PrimQuery.
   OR
   Implement an entirely new AST specially for Opaleye, including
   the outer join primative.
   (The former will be easier!)

2. Use the NullMaker technology below to allow the correct typing of
   the left join.
-}

import Data.Profunctor.Product.Default (Default, def)
import Karamaan.Opaleye.QueryColspec (QueryColspec)
import Karamaan.Opaleye.QueryArr (Query)
import qualified Karamaan.Opaleye.QueryArr as Q
import Karamaan.Opaleye.Wire (Wire)
import qualified Karamaan.Opaleye.Wire as Wire
import qualified Karamaan.Opaleye.Operators2 as Op2
import Control.Arrow (arr, returnA, (<<<), (***), (&&&))
import qualified Karamaan.Opaleye.Predicates as P
import Data.Profunctor (Profunctor, dimap)
import Data.Profunctor.Product (ProductProfunctor, empty, (***!))
import qualified Database.HaskellDB.PrimQuery as PQ
import qualified Karamaan.Opaleye.Unpackspec as U
import qualified Karamaan.Opaleye.SQL as SQL
import qualified Karamaan.Opaleye.ExprArr as E
import qualified Data.Profunctor.Product as PP

-- FIXME: this seems to fail on the union because the NULLs are not
-- given explicit types.  This is an annoyance of Postgres.  There'll be
-- a way to work around it (of course: just give the NULLs explicity types!)
-- but it is annoying.

-- NullMaker a b represents a way of turning a 'QueryArr z a' into a
-- 'QueryArr z b' where all the columns of 'b' are made nullable.
-- For example 'QueryArr (Wire Int, Wire Bool, Wire String)' could
-- become 'QueryArr (Wire (Maybe Int), Wire (Maybe Bool), Wire (Maybe String)'.
--
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

-- NB: We probably also need an instance
--
--       Default NullMaker (Wire (Maybe a)) (Wire (Maybe a))

instance Default NullMaker (Wire a) (Wire (Maybe a)) where
  def = NullMaker Wire.unsafeCoerce (Op2.constantLit PQ.NullLit)

instance Profunctor NullMaker where
  dimap f g nm = NullMaker (dimap f g (toNullable nm)) (fmap g (nulls nm))

instance ProductProfunctor NullMaker where
  empty = NullMaker id (arr id)
  NullMaker f n ***! NullMaker f' n' = NullMaker (f *** f') (n &&& n')

{-# DEPRECATED leftJoin "This was for exploratory purposes only" #-}
leftJoin :: (Default QueryColspec l l, Default QueryColspec r' r')
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

-- This is implemented using the trick of shimming arbitrary SQL into
-- a PrimQuery.  We use the BaseTable constructor and give it the SQL
-- rather than a table name.  This is not an ideal state of affairs,
-- but it was easier than adding left joins to PrimQuery.  Either we
-- will eventually add left joins to PrimQuery or we will just come up
-- with our own datatype.
leftJoinPP :: U.Unpackspec wiresA -> U.Unpackspec wiresB
              -> NullMaker wiresB wireNullablesB
              -> Query wiresA -> Query wiresB
              -> E.ExprArr (wiresA, wiresB) (Wire Bool)
              -> Query (wiresA, wireNullablesB)
leftJoinPP unpackA unpackB nullmaker qA qB expr = Q.simpleQueryArr f where
  f ((), startTag) = ((wiresA, wireNullablesB), primQueryR, endTag)
    where (wiresA, primQueryA, midTag) = Q.runQueryArrPrim' startTag unpackA qA
          (wiresB, primQueryB, endTag) = Q.runQueryArrPrim' midTag unpackB qB
          sqlExpr = SQL.formatAndShowSQLExpr primExpr where
            primExpr = E.runExprArr'' expr ((wiresA, wiresB), scope)
            scope = E.scopeUnion [scopeA, scopeB] where
              scopeA = E.scopeOfCols unpackA wiresA
              scopeB = E.scopeOfCols unpackB wiresB
          primQueryRS = "((" ++ sqlA ++ ") AS T1 LEFT OUTER JOIN (" ++ sqlB
                            ++ ") AS T2 ON " ++ sqlExpr ++ ")" where
            sqlA = SQL.optimizeFormatAndShowSQL primQueryA
            sqlB = SQL.optimizeFormatAndShowSQL primQueryB
          allCols = colsA ++ colsB where
            colsA = U.runUnpackspec unpackA wiresA
            colsB = U.runUnpackspec unpackB wiresB
          -- FIXME: ^^ maybe need to nub the cols
          primQueryR = PQ.Project projCols (PQ.BaseTable  primQueryRS allCols)
            where makeAssoc x = (x, PQ.AttrExpr x)
                  projCols = map makeAssoc allCols
            -- NB ^^ these are the same as in Table.hs
          wireNullablesB = toNullable nullmaker wiresB

leftJoin' :: (Default (PP.PPOfContravariant U.Unpackspec) wiresA wiresA,
              Default (PP.PPOfContravariant U.Unpackspec) wiresB wiresB,
              Default NullMaker wiresB wireNullablesB)
             => Query wiresA -> Query wiresB
              -> E.ExprArr (wiresA, wiresB) (Wire Bool)
              -> Query (wiresA, wireNullablesB)
leftJoin' = leftJoinPP (PP.unPPOfContravariant def)
                       (PP.unPPOfContravariant def)
                       def
