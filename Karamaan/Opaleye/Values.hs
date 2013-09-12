module Karamaan.Opaleye.Values where

import Karamaan.Opaleye.QueryArr (Query)
import Karamaan.Opaleye.Colspec (col)
import Karamaan.Opaleye.Table (makeTable)
import Data.List (intercalate)
import Karamaan.Opaleye.Wire (Wire)
import Control.Arrow ((***))
import Control.Monad.State (State, get, put, runState)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Karamaan.Opaleye.Colspec (Colspec, colsT2)
import qualified Karamaan.WhaleUtil.Date as UD
import Data.Time.Calendar
import Karamaan.Opaleye.Predicates (singleEnquoten)
import Control.Applicative (liftA2)

type S a = ReaderT String (State Int) a

data ValuesMaker a b = ValuesMaker (S (a -> [String], Colspec b))

--bimap :: (a' -> a) -> (b -> b' ) -> ValuesMaker a b -> ValuesMaker a' b'
--bimap f g (ValuesMaker p q) = ValuesMaker (fmap (. f) p) (fmap g q)

(****) :: ValuesMaker a b -> ValuesMaker a' b' -> ValuesMaker (a, a') (b, b')
(****) (ValuesMaker x) (ValuesMaker x') = ValuesMaker (combine x x')
  where combine = liftA2 (\(f, m) (f', m') -> (catResults f f', colsT2 (m, m')))

(.:.) :: (r -> z) -> (a -> b -> c -> r) -> (a -> b -> c -> z)
(f .:. g) x y z = f (g x y z)

catResults :: (a -> [r]) -> (c -> [r]) -> (a, c) -> [r]
catResults = uncurry (++) .:. (***)

nextCol :: S Int
nextCol = do { a <- get; put (a + 1); return a }

string :: ValuesMaker String (Wire String)
string = valuesMakerMaker singleEnquoten

int :: ValuesMaker Int (Wire Int)
int = valuesMakerMaker show

day :: ValuesMaker Day (Wire Day)
day = valuesMakerMaker dayToSQL

valuesMakerMaker :: (a -> String) -> ValuesMaker a (Wire b)
valuesMakerMaker f = ValuesMaker w
  where w = do { s <- ask; a <- nextCol; return ((:[]) . f, col (s ++ show a)) }

-- TODO: this doesn't belong here
dayToSQL :: Day -> String
dayToSQL = (++ " :: date") . singleEnquoten . UD.dayToSQL

-- colsT0 doesn't exist, but if it did I think this would work
--unit :: ValuesMaker () ()
--unit = ValuesMaker (return (const [])) (return colsT0)

run :: ValuesMaker a b -> String -> [a] -> ([[String]], Colspec b, Int)
run (ValuesMaker x) colPrefix a = (stringRows, colspec, nextCol')
  where startColNum = 1
        ((mapper, colspec), nextCol') = runS x colPrefix startColNum
        stringRows = map mapper a

runS :: S a -> String -> Int -> (a, Int)
runS m c s = runState (runReaderT m c) s

valuesToQuery' :: ([[String]], Colspec b, Int) -> Query b
valuesToQuery' (stringRows, colspec, nextCol') = makeTable colspec select
  where columnSelectors = map (colNames . show) [1..nextCol'-1]
        colNames x = "column" ++ x ++ " as foocol" ++ x
        select = (embracket . intercalate " ") [ "select"
                                               , intercalate "," columnSelectors
                                               , "from"
                                               , valuesOfStringRows stringRows
                                               , "as foo" ]

valueOfStringRow :: [String] -> String
valueOfStringRow = embracket . intercalate ","

valuesOfStringRows :: [[String]] -> String
valuesOfStringRows = embracket
                     . ("values "++)
                     . intercalate ","
                     . map valueOfStringRow

valuesToQuery :: ValuesMaker a b -> String -> [a] -> Query b
valuesToQuery v colPrefix rows = valuesToQuery' (run v colPrefix rows)

embracket :: String -> String
embracket = ("("++) . (++")")
