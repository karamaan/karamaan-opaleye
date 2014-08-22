{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

module Test where

import Karamaan.Opaleye.Wire (Wire)
import qualified Karamaan.Opaleye.Table as T
import qualified Karamaan.Opaleye.Manipulation as M
import qualified Karamaan.Opaleye.MakeExpr as ME
import qualified Database.PostgreSQL.Simple as SQL
import qualified Karamaan.Opaleye.ExprArr as E
import qualified Karamaan.Opaleye.QueryArr as Q
import qualified Karamaan.Opaleye.RunQuery as RQ
import qualified Karamaan.Opaleye.Distinct as Dis
import qualified Karamaan.Opaleye.Operators2 as O
import qualified Karamaan.Opaleye.Order as Order
import qualified Karamaan.Opaleye.Predicates as P
import qualified Karamaan.Opaleye.Aggregate as Agg
import qualified Karamaan.Opaleye.Nullable as N
import qualified Karamaan.Opaleye.Values as V
import qualified Karamaan.Opaleye.LeftJoin as LJ
import qualified Data.Profunctor.Product.Default as D
import qualified Data.Profunctor.Product as PP
import qualified Data.List as L
import qualified Data.Ord as Ord
import qualified Data.String as St
import Control.Arrow ((<<<))
import qualified Control.Arrow as Arr

-- { Set your test database info here.  Then invoke the 'run' function
--   to run the tests.  The test database must already exist and the
--   test user must have permissions to modify it.

connectInfo :: SQL.ConnectInfo
connectInfo =  SQL.ConnectInfo { SQL.connectHost = "Name of Postgres server"
                               , SQL.connectPort = 5432
                               , SQL.connectUser = "The test user username"
                               , SQL.connectPassword = "The test user password"
                               , SQL.connectDatabase = "The test database" }

-- }

twoIntTable :: String -> T.Table (Wire Int, Wire Int)
twoIntTable n = T.tableOfTableSpecDef (T.TableSpec ("column1", "column2") n)

table1 :: T.Table (Wire Int, Wire Int)
table1 = twoIntTable "table1"

table2 :: T.Table (Wire Int, Wire Int)
table2 = twoIntTable "table2"

table3 :: T.Table (Wire Int, Wire Int)
table3 = twoIntTable "table3"

table1Q :: Q.Query (Wire Int, Wire Int)
table1Q = T.queryTable table1

table2Q :: Q.Query (Wire Int, Wire Int)
table2Q = T.queryTable table2

table3Q :: Q.Query (Wire Int, Wire Int)
table3Q = T.queryTable table3

table1data :: [(Int, Int)]
table1data = [ (1, 100)
             , (1, 100)
             , (1, 200)
             , (2, 300) ]

table2data :: [(Int, Int)]
table2data = [ (1, 100)
             , (3, 400) ]

table3data :: [(Int, Int)]
table3data = [ (1, 50) ]

table1Just :: (Int, Int) -> E.Expr (Maybe (Wire Int), Maybe (Wire Int))
table1Just = ME.makeJustExpr

dropAndCreateTable :: String -> SQL.Query
dropAndCreateTable t = St.fromString ("DROP TABLE IF EXISTS " ++ t ++ ";"
                                      ++ "CREATE TABLE " ++ t
                                      ++ " (column1 integer, column2 integer);")

dropAndCreateDB :: SQL.Connection -> IO ()
dropAndCreateDB conn = do
  mapM_ execute ["table1", "table2", "table3"]
  where execute = SQL.execute_ conn . dropAndCreateTable

testG :: D.Default RQ.QueryRunner wires haskells =>
         Q.Query wires
         -> ([haskells] -> b)
         -> SQL.Connection
         -> IO b
testG q p conn = do
  result <- RQ.runQueryDefault q conn
  return (p result)

type Test = SQL.Connection -> IO Bool

testSelect :: Test
testSelect = testG table1Q
             (\r -> L.sort table1data == L.sort r)

testDistinct :: Test
testDistinct = testG (Dis.distinctBetter table1Q)
               (\r -> L.sort (L.nub table1data) == L.sort r)

testRestrict :: Test
testRestrict = testG query
               (\r -> filter ((== 1) . fst) (L.sort table1data) == L.sort r)
  where query = proc () -> do
          t <- table1Q -< ()
          one <- O.constant 1 -< ()
          P.restrict <<< O.eq -< (fst t, one)
          Arr.returnA -< t

-- FIXME: the unsafeCoerce is currently needed because the type
-- changes required for aggregation are not currently dealt with by
-- Opaleye.
testAggregate :: Test
testAggregate = testG ((Arr.second N.unsafeCoerce
                        <<< Agg.aggregate (PP.p2 (Agg.groupBy, Agg.sum))
                                          (T.queryTable table1))
                       :: Q.Query (Wire Int, Wire Integer))

                      (\r -> [(1, 400) :: (Int, Integer), (2, 300)] == L.sort r)

testOrderBy :: Test
testOrderBy = testG (Order.orderBy (Order.desc (Arr.arr snd))
                                   (T.queryTable table1))
                    (L.sortBy (flip (Ord.comparing snd)) table1data ==)

testLimit :: Test
testLimit = testG (Order.limit 2 (Order.orderBy (Order.desc (Arr.arr snd))
                                                (T.queryTable table1)))
                  (take 2 (L.sortBy (flip (Ord.comparing snd)) table1data) ==)


testUnionAll :: Test
testUnionAll = testG (table1Q `O.unionAll` table2Q)
                     (\r -> L.sort (table1data ++ table2data) == L.sort r)

testUnion :: Test
testUnion = testG (table1Q `O.union` table2Q)
                  (\r -> L.sort (L.nub (table1data ++ table2data)) == L.sort r)

difference :: Eq a => [a] -> [a] -> [a]
difference x y = filter (not . (`elem` y)) x

testDifference :: Test
testDifference = testG (table1Q `O.difference` table2Q)
                  (\r -> L.sort (L.nub (table1data `difference` table2data))
                         == L.sort r)

values :: [(Int, String, Bool)]
values = [ (1, "Hello",   True)
         , (2, "World",   False)
         , (3, "Goodbye", True)
         , (4, "World",   False) ]

testValues :: Test
testValues = testG (V.valuesToQuery (PP.p3 (V.int, V.string, V.bool)) values)
                   (values ==)

aLeftJoin :: Q.Query ((Wire Int, Wire Int),
                      (Wire (N.Nullable Int), Wire (N.Nullable Int)))
aLeftJoin = LJ.leftJoin' table1Q table3Q cond
  where cond = proc (l, r) -> do
          E.eq -< (fst l, fst r)

testLeftJoin :: Test
testLeftJoin = testG aLeftJoin (== expected)
  where expected :: [((Int, Int), (Maybe Int, Maybe Int))]
        expected = [ ((1, 100), (Just 1, Just 50))
                   , ((1, 100), (Just 1, Just 50))
                   , ((1, 200), (Just 1, Just 50))
                   , ((2, 300), (Nothing, Nothing)) ]

testLeftJoinNullable :: Test
testLeftJoinNullable = testG (joinNullable <<< q) (== expected)
  where q :: Q.Query ((Wire Int, Wire Int),
                       ((Wire (N.Nullable Int), Wire (N.Nullable Int)),
                      (Wire (N.Nullable (N.Nullable Int)),
                       Wire (N.Nullable (N.Nullable Int)))))
        q = LJ.leftJoin' table3Q aLeftJoin cond

        joinNullable = Arr.second
                       (Arr.second
                        (E.toQueryArrDef N.joinNullable
                         Arr.*** E.toQueryArrDef N.joinNullable))

        cond = E.eq <<< Arr.arr (fst Arr.*** (fst . fst))

        expected :: [((Int, Int), ((Maybe Int, Maybe Int), (Maybe Int, Maybe Int)))]
        expected = [ ((1, 50), ((Just 1, Just 100), (Just 1, Just 50)))
                   , ((1, 50), ((Just 1, Just 100), (Just 1, Just 50)))
                   , ((1, 50), ((Just 1, Just 200), (Just 1, Just 50))) ]

testLeftJoinNullableClass :: Test
testLeftJoinNullableClass = const (return True)
{- This will work if and when we implement the NullMaker additions

testLeftJoinNullableClass = testG q (== expected)
  where q :: Q.Query ((Wire Int, Wire Int),
                       ((Wire (N.Nullable Int), Wire (N.Nullable Int)),
                      (Wire (N.Nullable Int),
                       Wire (N.Nullable Int))))
        q = LJ.leftJoin' table3Q aLeftJoin cond

        cond = E.eq <<< Arr.arr (fst Arr.*** (fst . fst))

        expected :: [((Int, Int), ((Maybe Int, Maybe Int), (Maybe Int, Maybe Int)))]
        expected = [ ((1, 50), ((Just 1, Just 100), (Just 1, Just 50)))
                   , ((1, 50), ((Just 1, Just 100), (Just 1, Just 50)))
                   , ((1, 50), ((Just 1, Just 200), (Just 1, Just 50))) ]
-}

testCaseG constant eq case_ toQueryArrDef = testG q (== expected)
  where q = toQueryArrDef cond <<< table1Q
        expected :: [(Int, Int)]
        expected = [(1, 2), (1, 2), (2, 1), (3, 3)]
        cond = proc (i, j) -> do
          one   <- constant 1 -< ()
          two   <- constant 2 -< ()
          three <- constant 3 -< ()

          hundred <- constant 100 -< ()

          eq100 <- eq -< (j, hundred)
          eq1   <- eq -< (i, one)

          case_ -< ([(eq100, (one, two)), (eq1, (two, one))], (three, three))

type CaseType arr = arr (Wire Int, Wire Int) (Wire Int, Wire Int)

testCase :: Test
testCase = testCaseG E.constant E.eq E.case_ E.toQueryArrDef

testCaseQuery :: Test
testCaseQuery = testCaseG O.constant O.eq O.caseDef id

allTests :: [Test]
allTests = [testSelect, testDistinct, testRestrict, testAggregate, testOrderBy,
            testLimit, testUnionAll, testUnion, testDifference, testValues,
            testLeftJoin, testLeftJoinNullable, testLeftJoinNullableClass,
            testCase, testCaseQuery]

run :: IO ()
run = do
  conn <- SQL.connect connectInfo

  let insert (table, tabledata) =
        mapM_ (M.runInsertConnDef conn table . table1Just) tabledata

  dropAndCreateDB conn

  mapM_ insert [ (table1, table1data)
               , (table2, table2data)
               , (table3, table3data) ]

  results <- mapM ($ conn) allTests

  print results

  putStrLn (if and results then "All passed" else "Failure")
