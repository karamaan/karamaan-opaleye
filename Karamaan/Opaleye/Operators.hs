module Karamaan.Opaleye.Operators where

import Database.HaskellDB.Query (ShowConstant, showConstant)
import Database.HaskellDB.PrimQuery (PrimExpr(BinExpr, UnExpr,
                                              ConstExpr),
                                     BinOp, UnOp, Assoc)
import Karamaan.Opaleye.Wire (Wire(Wire))

unOp :: UnOp -> String -> PrimExpr -> String
          -> (String -> String)
          -> (Assoc, Wire a)
unOp op opname t t_string tag' =
  let newWireName = (tag' . concat) [opname, "_", t_string]
      newAssoc = [(newWireName, UnExpr op t)]
  in (newAssoc, Wire newWireName)

binOp' :: BinOp -> String -> PrimExpr -> String -> PrimExpr -> String
          -> (String -> String)
          -> (Assoc, Wire a)
binOp' op opname t t_string t' t'_string tag' =
  let newWireName = (tag' . concat) [t_string, "_", opname, "_", t'_string]
      newAssoc = [(newWireName, BinExpr op t t')]
  in (newAssoc, Wire newWireName)

constant :: ShowConstant a => a -> PrimExpr
constant = ConstExpr . showConstant
