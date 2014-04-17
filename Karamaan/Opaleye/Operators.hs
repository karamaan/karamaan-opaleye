module Karamaan.Opaleye.Operators where

import Database.HaskellDB.Query (ShowConstant, showConstant)
import Database.HaskellDB.PrimQuery (PrimExpr(BinExpr, ConstExpr),
                                     BinOp, Assoc)
import Karamaan.Opaleye.Wire (Wire(Wire))

-- TODO vv I put this take 5 in here because the query strings were getting
-- too long and postgres was complaining that it was truncating them.
-- This is really just a temporary fix, because I'd like to keep the
-- possibility of long names but postprocess the PrimQuery to shorten
-- them before sending them to postgres.
operatorName :: String -> String -> String -> String
operatorName left opName right = concat [t left, "_", opName, "_", t right]
  where t = take 5

binOp' :: BinOp -> String -> PrimExpr -> String -> PrimExpr -> String
          -> (String -> String)
          -> (Assoc, Wire a)
binOp' op opname t t_string t' t'_string tag' =
  let newWireName = tag' (operatorName t_string opname t'_string)
      newAssoc = [(newWireName, BinExpr op t t')]
  in (newAssoc, Wire newWireName)

-- If you think you want to use this, you're probably better off with
-- Operators2.constant.  (Sorry for the confusing naming scheme).
constant :: ShowConstant a => a -> PrimExpr
constant = ConstExpr . showConstant
