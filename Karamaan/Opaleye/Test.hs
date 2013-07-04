{-# LANGUAGE Arrows #-}

module Karamaan.Opaleye.Test where

import Karamaan.Opaleye.Colspec (cols4, col)
import Karamaan.Opaleye.Table (makeTable)
import Karamaan.Opaleye.QueryArr (runQueryArrPrim, Query)
import Karamaan.Opaleye.Wire (Wire)
import Karamaan.Opaleye.Operators2 (eq)

import Database.HaskellDB.PrimQuery (PrimQuery(Project, BaseTable,
                                               Restrict, Group, Binary,
                                               Special, Empty),
                                     BinOp(OpEq),
                                     RelOp(Times, Union, Intersect,
                                           Divide, Difference),
                                     PrimExpr(AttrExpr, BinExpr))

import Test.HUnit (Test(TestCase, TestList, TestLabel), assertEqual, runTestTT)

table :: Query (Wire Int, Wire Int, Wire Bool, Wire Double)
table = makeTable (cols4 ( col "int1"
                         , col "int2"
                         , col "bool"
                         , col "double" ) )
        "test_table"

tablePrimQ = (Project [ ("int11", AttrExpr "int1")
                      , ("int21", AttrExpr "int2")
                      , ("bool1", AttrExpr "bool")
                      , ("double1", AttrExpr "double")]
              (BaseTable "test_table" [ "int1"
                                      , "int2"
                                      , "bool"
                                      , "double" ]))

test1 = TestCase (assertEqual "table" (Project [ ("int11", AttrExpr "int11")
                                               , ("int21", AttrExpr "int21")
                                               , ("bool1", AttrExpr "bool1")
                                               , ("double1", AttrExpr "double1")]
                                       tablePrimQ)
                                      (runQueryArrPrim table))

testEq = proc () -> do
  (i, i', _, _) <- table -< ()
  eq -< (i, i')

test2 = TestCase (assertEqual "eq" (Project [("int11_eq_int212", AttrExpr "int11_eq_int212")]
                                    (Project [("int11_eq_int212",
                                               BinExpr OpEq (AttrExpr "int11") (AttrExpr "int21")),
                                              ("int11",
                                               AttrExpr "int11"),
                                              ("int21",
                                               AttrExpr "int21"),
                                              ("bool1",
                                               AttrExpr "bool1"),
                                              ("double1",
                                               AttrExpr "double1")]
                                     (Project [("int11",AttrExpr "int1"),
                                               ("int21",AttrExpr "int2"),
                                               ("bool1",AttrExpr "bool"),
                                               ("double1",AttrExpr "double")]
                                      (BaseTable "test_table" ["int1","int2",
                                                               "bool","double"]))))
                                   (runQueryArrPrim testEq))

tests = TestList [ TestLabel "test1" test1
                 , TestLabel "test2" test2 ]

main = runTestTT tests
