{-# LANGUAGE Arrows #-}

module Karamaan.Opaleye.Test where

import Karamaan.Opaleye.Colspec (cols4, col)
import Karamaan.Opaleye.Table (makeTable)
import Karamaan.Opaleye.QueryArr (runQueryArrPrim, Query)
import Karamaan.Opaleye.Wire (Wire)
import Karamaan.Opaleye.Operators2 (eq, gt, minus, constant, constantString)

import Database.HaskellDB.PrimQuery (PrimQuery(Project, BaseTable,
                                               Restrict, Group, Binary,
                                               Special, Empty),
                                     Literal(IntegerLit, DoubleLit, OtherLit),
                                     BinOp(OpEq, OpGt, OpMinus),
                                     RelOp(Times, Union, Intersect,
                                           Divide, Difference),
                                     PrimExpr(AttrExpr, BinExpr, ConstExpr))

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

tableAssocs = [ ("int11", AttrExpr "int11")
              , ("int21", AttrExpr "int21")
              , ("bool1", AttrExpr "bool1")
              , ("double1", AttrExpr "double1")]

test1 = TestCase (assertEqual "table" (Project tableAssocs tablePrimQ)
                                      (runQueryArrPrim table))
testOp op = proc () -> do
  (i, i', _, _) <- table -< ()
  op -< (i, i')

testeq = TestCase (assertEqual "eq" (Project [("int11_eq_int212",
                                              AttrExpr "int11_eq_int212")]
                                    (Project ([("int11_eq_int212",
                                               BinExpr OpEq (AttrExpr "int11")
                                                            (AttrExpr "int21"))]
                                              ++ tableAssocs)
                                     tablePrimQ))
                                   (runQueryArrPrim (testOp eq)))

testgt = TestCase (assertEqual "gt" (Project [("int11_gt_int212",
                                              AttrExpr "int11_gt_int212")]
                                    (Project ([("int11_gt_int212",
                                               BinExpr OpGt (AttrExpr "int11")
                                                            (AttrExpr "int21"))]
                                              ++ tableAssocs)
                                     tablePrimQ))
                                   (runQueryArrPrim (testOp gt)))

testminus = TestCase (assertEqual "minus" (Project [("int11_minus_int212",
                                              AttrExpr "int11_minus_int212")]
                                    (Project ([("int11_minus_int212",
                                               BinExpr OpMinus (AttrExpr "int11")
                                                            (AttrExpr "int21"))]
                                              ++ tableAssocs)
                                     tablePrimQ))
                                   (runQueryArrPrim (testOp minus)))

constantint = Project [("constant1",AttrExpr "constant1")]
              (Project [("constant1",ConstExpr (IntegerLit 1))] Empty)

constantdouble = Project [("constant1",AttrExpr "constant1")]
              (Project [("constant1",ConstExpr (DoubleLit 1))] Empty)

constantstring = Project [("constant1",AttrExpr "constant1")]
                 (Project [("constant1",ConstExpr (OtherLit "'hello' :: text"))]
                  Empty)

testconstantint = TestCase (assertEqual "constint" constantint
                            (runQueryArrPrim (constant (1 :: Int))))

testconstantdouble = TestCase (assertEqual "constdouble" constantdouble
                               (runQueryArrPrim (constant (1 :: Double))))

testconstantstring = TestCase (assertEqual "conststring" constantstring
                               (runQueryArrPrim (constantString "hello")))

tests = TestList [ test1
                 , testeq
                 , testgt
                 , testminus
                 , testconstantint
                 , testconstantdouble
                 , testconstantstring
                 ]

main = runTestTT tests
