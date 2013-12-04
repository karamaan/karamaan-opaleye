{-# LANGUAGE Arrows #-}

module Karamaan.Opaleye.ManipulationExample where

import Prelude hiding (or)
import Karamaan.Opaleye.Wire (Wire(Wire))
import Karamaan.Opaleye.ExprArr (ExprArr, Expr, eq, plus, mul, constant, or)
import Database.HaskellDB.Sql.Print (ppDelete, ppInsert, ppUpdate)
import Control.Arrow (returnA)
import Karamaan.Opaleye.Manipulation (arrangeDeleteDef,
                                      arrangeInsertDef, arrangeUpdateDef)
import Karamaan.Opaleye.Table (Table(Table))

table :: Table ((Wire Int, Wire Int), Wire Int)
table = Table "tablename" ((Wire "col1", Wire "col2"), Wire "col3")

testDelete :: String
testDelete = show (ppDelete sqlDelete')
  where condExpr :: ExprArr ((Wire Int, Wire Int), Wire Int) (Wire Bool)
        condExpr = proc ((x, y), z) -> do
          x_plus_y <- plus -< (x, y)
          cond1 <- eq -< (x_plus_y, z)
          cond2 <- eq -< (x, z)
          or -< (cond1, cond2)
        sqlDelete' = arrangeDeleteDef table condExpr

testInsert :: String
testInsert = show (ppInsert sqlInsert')
  where insertExpr :: Expr ((Maybe (Wire Int), Maybe (Wire Int)),
                            Maybe (Wire Int))
        insertExpr = proc () -> do
          one <- constant 1 -< ()
          five <- constant 5 -< ()
          six <- constant 6 -< ()

          five_plus_six <- plus -< (five, six)

          returnA -< ((Just one, Nothing), Just five_plus_six)
        sqlInsert' = arrangeInsertDef table insertExpr

testUpdate :: String
testUpdate = show (ppUpdate sqlUpdate')
  where updateExpr :: ExprArr ((Wire Int, Wire Int),
                               Wire Int)
                              ((Maybe (Wire Int), Maybe (Wire Int)),
                               Maybe (Wire Int))
        updateExpr = proc ((x, y), _) -> do
          x_plus_y <- plus -< (x, y)
          returnA -< ((Nothing, Just x_plus_y), Nothing)
        condExpr :: ExprArr ((Wire Int, Wire Int), Wire Int) (Wire Bool)
        condExpr = proc ((x, _), z) -> do
          eq -< (x, z)
        sqlUpdate' = arrangeUpdateDef table updateExpr condExpr

testTable :: Table (Wire Int, Wire Int, Wire Int)
testTable = Table "test_table" (Wire "id", Wire "col1", Wire "col2")

testTableInsert :: String
testTableInsert = show (ppInsert sqlInsert')
  where insertExpr :: Expr (Maybe (Wire Int), Maybe (Wire Int),  Maybe (Wire Int))
        insertExpr = proc () -> do
          five <- constant 5 -< ()
          six <- constant 6 -< ()
          returnA -< (Nothing, Just five, Just six)
        sqlInsert' = arrangeInsertDef testTable insertExpr

testTableDelete :: String
testTableDelete = show (ppDelete sqlDelete')
  where condExpr :: ExprArr (Wire Int, Wire Int, Wire Int) (Wire Bool)
        condExpr = proc (x, _, z) -> do
          two <- (constant 2 :: Expr (Wire Int)) -< ()
          x_mul_2 <- mul -< (x, two)
          eq -< (x_mul_2, z)
        sqlDelete' = arrangeDeleteDef testTable condExpr

testTableUpdate :: String
testTableUpdate = show (ppUpdate sqlUpdate')
  where updateExpr :: ExprArr (Wire Int, Wire Int, Wire Int)
                              (Maybe (Wire Int), Maybe (Wire Int),
                               Maybe (Wire Int))
        updateExpr = proc (x, y, _) -> do
          x_plus_y <- plus -< (x, y)
          returnA -< (Nothing, Nothing, Just x_plus_y)
        condExpr :: ExprArr (Wire Int, Wire Int, Wire Int) (Wire Bool)
        condExpr = proc (x, _, _) -> do
          five <- constant 5 -< ()
          eq -< (x, five)
        sqlUpdate' = arrangeUpdateDef testTable updateExpr condExpr
