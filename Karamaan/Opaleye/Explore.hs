{-# LANGUAGE Arrows #-}

module Karamaan.Opaleye.Explore where

import Karamaan.Opaleye.Table (makeTable)
import Karamaan.Opaleye.Colspec (col)
import Karamaan.Opaleye.Applicative (a1)
import Karamaan.Opaleye.QueryArr (equals, Query)
import Karamaan.Opaleye.SQL (showSqlForPostgreSQLSimple, showSqlUnopt)
import Karamaan.Opaleye.Predicates (equalsC)
import Control.Arrow (returnA)
import Karamaan.Opaleye.Wire (Wire)
import Karamaan.Opaleye.Operators2 (union, constant)
import Karamaan.Opaleye.Operators.Numeric (plus, divide)

exampleTable :: Query (Wire Int)
exampleTable = makeTable (a1 (col "id")) "exampleTable"

-- Happily the optimised versions of these are equal.  This means that
-- equals could be implemented in terms of constant and equals.
example1 :: Query (Wire Int)
example1 = proc () -> do
  id' <- exampleTable -< ()
  equalsC 1 -< id'
  returnA -< id'

example2 :: Query (Wire Int)
example2 = proc () -> do
  id' <- exampleTable -< ()
  id'' <- constant 1 -< ()
  equals -< (id', id'')
  returnA -< id'

run1_opt :: String
run1_opt = showSqlForPostgreSQLSimple example1

run2_opt :: String
run2_opt = showSqlForPostgreSQLSimple example2

run1_unopt :: String
run1_unopt = showSqlUnopt example1

run2_unopt :: String
run2_unopt = showSqlUnopt example2

-- It's nice that the optimizer folds all these operations in to one, but
-- it also comes out with incorrect bracketing again!
--
-- "SELECT id + 1 / 2 as ..."
--
-- Oh dear, HaskellDB
example3 :: Query (Wire Int)
example3 = proc () -> do
  id' <- exampleTable -< ()
  one <- constant 1 -< ()
  two <- constant 2 -< ()

  id_plus_one <- plus -< (id', one)
  div_two <- divide -< (id_plus_one, two)

  returnA -< div_two

run3_opt :: String
run3_opt = showSqlForPostgreSQLSimple example3

run3_unopt :: String
run3_unopt = showSqlUnopt example3

-- These constants unfortunately just come out as a bunch of unions
constants :: String
constants = showSqlForPostgreSQLSimple $
                foldr1 union (map constant [1..5 :: Int])
