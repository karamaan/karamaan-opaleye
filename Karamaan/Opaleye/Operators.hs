module Karamaan.Opaleye.Operators where

import Database.HaskellDB.Query (ShowConstant, showConstant)
import Database.HaskellDB.PrimQuery (PrimExpr(BinExpr,
                                              ConstExpr),
                                     BinOp, Assoc)
import Karamaan.Opaleye.Wire (Wire(Wire))

binOp' :: BinOp -> String -> PrimExpr -> String -> PrimExpr -> String
          -> (String -> String)
          -> (Assoc, Wire a)
binOp' op opname t t_string t' t'_string tag' =
  let newWireName = (tag' . concat) [t_string, "_", opname, "_", t'_string]
      newAssoc = [(newWireName, BinExpr op t t')]
  in (newAssoc, Wire newWireName)

constant :: ShowConstant a => a -> PrimExpr
constant = ConstExpr . showConstant
