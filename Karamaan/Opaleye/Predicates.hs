module Karamaan.Opaleye.Predicates where

import Karamaan.Opaleye.Wire (Wire, unWire)
import Karamaan.Opaleye.QueryArr (QueryArr, restrictWith)
import Karamaan.Opaleye.Values (sqlStringOfDay)
import Karamaan.Opaleye.Operators2 (eq, constantDay)
import Database.HaskellDB.Query (ShowConstant, showConstant)
import Database.HaskellDB.PrimQuery (PrimExpr(AttrExpr, UnExpr, ConstExpr,
                                              BinExpr),
                                     Literal(BoolLit),
                                     BinOp(OpOr, OpEq, OpNotEq),
                                     UnOp(OpIsNull), Literal(OtherLit))
import Data.Time.Calendar (Day)
import Control.Arrow (arr, Arrow, first, (<<<))
import qualified Karamaan.WhaleUtil.Arrow as UA

equalsOneOf :: ShowConstant a => [a] -> QueryArr (Wire a) ()
equalsOneOf = restrictWith . flip wireIsOneOf . map showConstant

doesntEqualAnyOf :: ShowConstant a => [a] -> QueryArr (Wire a) ()
doesntEqualAnyOf = UA.all_ . map notEqualC

notEqualC :: ShowConstant a => a -> QueryArr (Wire a) ()
notEqualC = restrictWith . flip wireIsNot . showConstant

-- TODO: replace this with something like equalsDay?
equalsC :: ShowConstant a => a -> QueryArr (Wire a) ()
equalsC = restrictWith . flip wireIs . showConstant

literalDay :: Day -> Literal
literalDay = OtherLit . sqlStringOfDay
                  -- ^^ I guess this should really be a DateLit, but I can't
                  -- work out how to use HaskellDB's CalendarTime

-- TODO: should we get rid of this as it is somewhat redundant?
equalsDay :: Day -> QueryArr (Wire Day) ()
equalsDay day = restrict
                <<< eq
                <<< first (constantDay day)
                <<< arr (\d -> ((), d))

-- FIXME: Should be (Wire (Maybe a))?
-- 'Predicates.isNull' existed before 'Operators2.isNull', and the following
-- condition is supposed to hold:
--
-- Predicates.isNull = restrict <<< Operators2.isNull
--
-- Prefer the right hand side in new code.  Predicates.isNull should probably
-- be removed.
{-# DEPRECATED isNull "Use 'restrict <<< Operators2.isNull' instead" #-}
isNull :: QueryArr (Wire a) ()
isNull = restrictWith null'

null' :: Wire a -> PrimExpr
null' = UnExpr OpIsNull . AttrExpr . unWire

wireIsOneOf :: Wire a -> [Literal] -> PrimExpr
wireIsOneOf w = foldr or' false . map (wireIs w)
  where false :: PrimExpr
        false = ConstExpr (BoolLit False)
        or' = BinExpr OpOr

wireOp :: BinOp -> Wire a -> Literal -> PrimExpr
wireOp op w l = BinExpr op ((AttrExpr . unWire) w) (ConstExpr l)

wireIs :: Wire a -> Literal -> PrimExpr
wireIs = wireOp OpEq

wireIsNot :: Wire a -> Literal -> PrimExpr
wireIsNot = wireOp OpNotEq

noOp :: Arrow arr => arr a ()
noOp = arr (const ())

wireTrue :: Wire a -> PrimExpr
wireTrue = AttrExpr . unWire

restrict :: QueryArr (Wire Bool) ()
restrict = restrictWith wireTrue
