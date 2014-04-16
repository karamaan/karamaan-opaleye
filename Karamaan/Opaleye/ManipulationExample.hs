{-# LANGUAGE Arrows #-}

module Karamaan.Opaleye.ManipulationExample where

import Prelude hiding (or)
import Karamaan.Opaleye.Wire (Wire(Wire))
import Karamaan.Opaleye.ExprArr (ExprArr, Expr, eq, plus, mul, constant, or)
import Control.Arrow (returnA)
import Karamaan.Opaleye.Manipulation (arrangeDeleteSqlDef,
                                      arrangeInsertSqlDef, arrangeUpdateSqlDef)
import qualified Karamaan.Opaleye.Manipulation as M
import Karamaan.Opaleye.Table (Table(Table))

table :: Table ((Wire Int, Wire Int), Wire Int)
table = Table "tablename" ((Wire "col1", Wire "col2"), Wire "col3")

testDelete :: String
testDelete = arrangeDeleteSqlDef table condExpr
  where condExpr :: ExprArr ((Wire Int, Wire Int), Wire Int) (Wire Bool)
        condExpr = proc ((x, y), z) -> do
          x_plus_y <- plus -< (x, y)
          cond1 <- eq -< (x_plus_y, z)
          cond2 <- eq -< (x, z)
          or -< (cond1, cond2)

testInsert :: String
testInsert = arrangeInsertSqlDef table insertExpr
  where insertExpr :: Expr ((Maybe (Wire Int), Maybe (Wire Int)),
                            Maybe (Wire Int))
        insertExpr = proc () -> do
          one <- constant 1 -< ()
          five <- constant 5 -< ()
          six <- constant 6 -< ()

          five_plus_six <- plus -< (five, six)

          returnA -< ((Just one, Nothing), Just five_plus_six)

testUpdate :: String
testUpdate = arrangeUpdateSqlDef table updateExpr condExpr
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

testTable :: Table (Wire Int, Wire Int, Wire Int)
testTable = Table "test_table" (Wire "id", Wire "col1", Wire "col2")

testTableInsert :: String
testTableInsert = arrangeInsertSqlDef testTable insertExpr
  where insertExpr :: Expr (Maybe (Wire Int), Maybe (Wire Int),  Maybe (Wire Int))
        insertExpr = proc () -> do
          five <- constant 5 -< ()
          six <- constant 6 -< ()
          returnA -< (Nothing, Just five, Just six)

testTableInsertReturning :: String
testTableInsertReturning = M.arrangeInsertReturningSqlDef testTable insertExpr
                                                          returnExpr
  where insertExpr :: Expr (Maybe (Wire Int), Maybe (Wire Int),  Maybe (Wire Int))
        insertExpr = proc () -> do
          five <- constant 5 -< ()
          six <- constant 6 -< ()
          returnA -< (Nothing, Just five, Just six)
        returnExpr :: ExprArr (Wire Int, Wire Int,  Wire Int)
                              (Wire Bool, Wire Int)
        returnExpr = proc (a, b, c) -> do
          eq'  <- eq   -< (a, b)
          sum' <- plus -< (b, c)
          returnA -< (eq', sum')

testTableDelete :: String
testTableDelete = arrangeDeleteSqlDef testTable condExpr
  where condExpr :: ExprArr (Wire Int, Wire Int, Wire Int) (Wire Bool)
        condExpr = proc (x, _, z) -> do
          two <- (constant 2 :: Expr (Wire Int)) -< ()
          x_mul_2 <- mul -< (x, two)
          eq -< (x_mul_2, z)

testTableUpdate :: String
testTableUpdate = arrangeUpdateSqlDef testTable updateExpr condExpr
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
