module Karamaan.Opaleye.Predicates where

import Karamaan.Opaleye.Wire (Wire, unWire)
import Karamaan.Opaleye.QueryArr (QueryArr, restrictWith)
import Database.HaskellDB.Query (ShowConstant, showConstant)
import Database.HaskellDB.PrimQuery (PrimExpr(AttrExpr, UnExpr, ConstExpr,
                                              BinExpr),
                                     Literal(BoolLit),
                                     BinOp(OpOr, OpEq, OpNotEq),
                                     UnOp(OpIsNull), Literal(OtherLit))
import Data.Time.Calendar (Day)
import qualified Karamaan.WhaleUtil.Date as UD
import Control.Arrow (arr, Arrow)
import qualified Karamaan.WhaleUtil.Arrow as UA

equalsOneOf :: ShowConstant a => [a] -> QueryArr (Wire a) ()
equalsOneOf = restrictWith . flip wireIsOneOf . map showConstant

doesntEqualAnyOf :: ShowConstant a => [a] -> QueryArr (Wire a) ()
doesntEqualAnyOf = UA.all_ . map notEqualC

notEqualC :: ShowConstant a => a -> QueryArr (Wire a) ()
notEqualC = restrictWith . flip wireIsNot . showConstant

equalsC :: ShowConstant a => a -> QueryArr (Wire a) ()
equalsC = restrictWith . flip wireIs . showConstant

literalDay :: Day -> Literal
literalDay = OtherLit . singleEnquoten . UD.dayToSQL
                  -- ^^ I guess this should really be a DateLit, but I can't
                  -- work out how to use HaskellDB's CalendarTime

-- We only need equalsDay because HaskellDB insists on using CalendarTime
-- for some reason
equalsDay :: Day -> QueryArr (Wire Day) ()
equalsDay = restrictWith . flip wireIs . literalDay

-- FIXME: Should be (Wire (Maybe a))?
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

-- TODO: this should really be put somewhere else, or perhaps
-- singleEnquoten . UD.dayToSQL should be
singleEnquoten :: String -> String
singleEnquoten = ("'" ++) . (++"'")

noOp :: Arrow arr => arr a ()
noOp = arr (const ())

wireTrue :: Wire a -> PrimExpr
wireTrue = AttrExpr . unWire

restrict :: QueryArr (Wire Bool) ()
restrict = restrictWith wireTrue
