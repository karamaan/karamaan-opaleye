module Karamaan.Opaleye.Test where

import Karamaan.Opaleye.Colspec (cols4, col)
import Karamaan.Opaleye.Table (makeTable)
import Karamaan.Opaleye.QueryArr (runQueryArrPrim, Query)
import Karamaan.Opaleye.Wire (Wire)

import Database.HaskellDB.PrimQuery (PrimQuery(Project, BaseTable,
                                               Restrict, Group, Binary,
                                               Special, Empty),
                                     RelOp(Times, Union, Intersect,
                                           Divide, Difference),
                                     PrimExpr(AttrExpr))

import Test.HUnit (Test(TestCase, TestList, TestLabel), assertEqual, runTestTT)

table :: Query (Wire Int, Wire Int, Wire Bool, Wire Double)
table = makeTable (cols4 ( col "int1"
                         , col "int2"
                         , col "bool"
                         , col "double" ) )
        "test_table"

test1 = TestCase (assertEqual "table" (Project [ ("int11", AttrExpr "int11")
                                               , ("int21", AttrExpr "int21")
                                               , ("bool1", AttrExpr "bool1")
                                               , ("double1", AttrExpr "double1")]
                                       (Project [ ("int11", AttrExpr "int1")
                                                , ("int21", AttrExpr "int2")
                                                , ("bool1", AttrExpr "bool")
                                                , ("double1", AttrExpr "double")]
                                        (BaseTable "test_table" [ "int1"
                                                                , "int2"
                                                                , "bool"
                                                                , "double" ])))
                                      (runQueryArrPrim table))

tests = TestList [ TestLabel "test1" test1 ]

main = runTestTT tests
