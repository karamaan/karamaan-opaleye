module Karamaan.Opaleye.Predicates where

import Karamaan.Opaleye.Wire (Wire, unWire)
import Karamaan.Opaleye.QueryArr (QueryArr, restrictWith)
import Karamaan.Opaleye.Operators2 (eq, constantDay)
import Database.HaskellDB.Query (ShowConstant, showConstant)
import Database.HaskellDB.PrimQuery (PrimExpr(AttrExpr, ConstExpr,
                                              BinExpr),
                                     Literal,
                                     BinOp(OpEq, OpNotEq),
                                     Literal)
import Data.Time.Calendar (Day)
import Control.Arrow (arr, first, (<<<))

-- The only useful function here is 'restrict'.  All the others should
-- be considered deprecated.  Use 'Karamaan.Opaleye.ExprArr' instead.

restrict :: QueryArr (Wire Bool) ()
restrict = restrictWith wireTrue

-- WARNING!
--
-- ALL THE FOLLOWING ARE TO BE CONSIDERED DEPRECATED

notEqualC :: ShowConstant a => a -> QueryArr (Wire a) ()
notEqualC = restrictWith . flip wireIsNot . showConstant

-- TODO: replace this with something like equalsDay?
equalsC :: ShowConstant a => a -> QueryArr (Wire a) ()
equalsC = restrictWith . flip wireIs . showConstant

-- TODO: should we get rid of this as it is somewhat redundant?
equalsDay :: Day -> QueryArr (Wire Day) ()
equalsDay day = restrict
                <<< eq
                <<< first (constantDay day)
                <<< arr (\d -> ((), d))

wireOp :: BinOp -> Wire a -> Literal -> PrimExpr
wireOp op w l = BinExpr op ((AttrExpr . unWire) w) (ConstExpr l)

wireIs :: Wire a -> Literal -> PrimExpr
wireIs = wireOp OpEq

wireIsNot :: Wire a -> Literal -> PrimExpr
wireIsNot = wireOp OpNotEq

wireTrue :: Wire a -> PrimExpr
wireTrue = AttrExpr . unWire
