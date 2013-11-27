module Karamaan.Opaleye.ManipulationExample where

import Prelude hiding (or)
import Karamaan.Opaleye.Wire (Wire(Wire))
import Karamaan.Opaleye.ExprArr (ExprArr, Expr, eq, plus, constant, or)
import Data.Profunctor.Product (unPPOfContravariant)
import Database.HaskellDB.Sql.Print (ppDelete, ppInsert, ppUpdate)
import Control.Arrow ((&&&), (<<<), first, arr)
import Karamaan.Opaleye.Default (Default, def)
import Karamaan.Opaleye.Manipulation (Table(Table), arrangeDelete,
                                      arrangeInsert, arrangeUpdate)

testDelete :: String
testDelete = show (ppDelete sqlDelete')
  where table :: Table ((Wire Int, Wire Int), Wire Int)
        table = Table "tablename" ((Wire "col1", Wire "col2"), Wire "col3")
        condExpr :: ExprArr ((Wire Int, Wire Int), Wire Int) (Wire Bool)
        condExpr = or <<< ((eq <<< first plus) &&& (eq <<< arr (fst . fst) &&& arr snd))
        sqlDelete' = arrangeDelete def table condExpr

testInsert :: String
testInsert = show (ppInsert sqlInsert')
  where table :: Table ((Wire Int, Wire Int), Wire Int)
        table  = Table "tablename" ((Wire "col1", Wire "col2"), Wire "col3")
        insertExpr :: Expr ((Maybe (Wire Int), Maybe (Wire Int)),
                            Maybe (Wire Int))
        insertExpr = ((arr Just <<< constant 1)
                      &&& (arr (const Nothing)))
                     &&& (arr Just <<< plus <<< (constant 5 &&& constant 6))
        sqlInsert' = arrangeInsert def' def table insertExpr
        def' = unPPOfContravariant def

testUpdate :: String
testUpdate = show (ppUpdate sqlUpdate')
  where table :: Table ((Wire Int, Wire Int), Wire Int)
        table  = Table "tablename" ((Wire "col1", Wire "col2"), Wire "col3")
        updateExpr :: ExprArr ((Wire Int, Wire Int),
                               Wire Int)
                              ((Maybe (Wire Int), Maybe (Wire Int)),
                               Maybe (Wire Int))
        updateExpr = (arr (const Nothing) &&& (arr Just <<< plus <<< arr fst))
                     &&& arr (const Nothing)
        condExpr :: ExprArr ((Wire Int, Wire Int), Wire Int) (Wire Bool)
        condExpr = eq <<< arr ((fst . fst) &&& snd)

        sqlUpdate' = arrangeUpdate def def' def table updateExpr condExpr
        def' = unPPOfContravariant def
