module Karamaan.Opaleye.Values where

import Karamaan.Opaleye.QueryArr (Query)
import Karamaan.Opaleye.Colspec (col)
import Karamaan.Opaleye.Table (makeTable)
import Data.List (intercalate)
import Karamaan.Opaleye.Wire (Wire)
import Control.Arrow ((***))
import Control.Monad.State (State, get, put, evalState, execState)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Applicative (liftA2)
import Karamaan.Opaleye.Colspec (Colspec, colsT2)
import qualified Karamaan.WhaleUtil.Date as UD
import Data.Time.Calendar
import Karamaan.Opaleye.Predicates (singleEnquoten)

-- TODO: this is too big.  The String should just be a reader
type S a = ReaderT String (State Int) a

data ValuesMaker a b = ValuesMaker (S (a -> [String])) (S (Colspec b))

--bimap :: (a' -> a) -> (b -> b' ) -> ValuesMaker a b -> ValuesMaker a' b'
--bimap f g (ValuesMaker p q) = ValuesMaker (fmap (. f) p) (fmap g q)

(****) :: ValuesMaker a b -> ValuesMaker a' b' -> ValuesMaker (a, a') (b, b')
(****) (ValuesMaker f m) (ValuesMaker f' m') = ValuesMaker f'' m''
  where f'' = liftA2 catResults f f'
        m'' = fmap colsT2 (liftA2 (,) m m')

(.:.) :: (r -> z) -> (a -> b -> c -> r) -> (a -> b -> c -> z)
(f .:. g) x y z = f (g x y z)

catResults :: (a -> [r]) -> (c -> [r]) -> (a, c) -> [r]
catResults = uncurry (++) .:. (***)

addOne :: S ()
addOne = do { a <- get; put (a + 1) }

string :: ValuesMaker String (Wire String)
string = ValuesMaker (addOne >> return (return . singleEnquoten)) w
  where w = do { s <- ask; a <- get; addOne; return (col (s ++ show a)) }

int :: ValuesMaker Int (Wire Int)
int = ValuesMaker (addOne >> return (return . show)) w
  where w = do { s <- ask; a <- get; addOne; return (col (s ++ show a)) }

day :: ValuesMaker Day (Wire Day)
day = ValuesMaker (addOne >> return (return . dayToSQL)) w
  where w = do { s <- ask; a <- get; addOne; return (col (s ++ show a)) }
        dayToSQL :: Day -> String
        dayToSQL = (++ " :: date") . singleEnquoten . UD.dayToSQL
        -- ^^ FIXME: duplication with constantDay

-- colsT0 doesn't exist, but if it did I think this would work
--unit :: ValuesMaker () ()
--unit = ValuesMaker (return (const [])) (return colsT0)

run :: ValuesMaker a b -> String -> [a] -> ([[String]], Colspec b, Int)
run (ValuesMaker f m) colPrefix a = (stringRows, colspec, nextCol)
  where startColNum = 1
        stringRows = map (evalS f colPrefix startColNum) a
        colspec = evalS m colPrefix startColNum
        nextCol = execS m colPrefix startColNum
        -- ^^ OK this is weird.  I guess we should only have
        -- one way of getting the number of columns shouldn't
        -- we?

evalS :: S a -> String -> Int -> a
evalS m c s = evalState (runReaderT m c) s

execS :: S a -> String -> Int -> Int
execS m c s = execState (runReaderT m c) s


valuesToQuery :: ValuesMaker a b -> String -> [a] -> Query b
valuesToQuery v colPrefix rows = makeTable colspec select
  where (stringRows, colspec, nextCol) = run v colPrefix rows
        valueOfStringRow :: [String] -> String
        valueOfStringRow = embracket . intercalate ","
        valuesOfStringRows :: [[String]] -> String
        valuesOfStringRows = embracket . ("values "++) . intercalate "," . map valueOfStringRow
        columnSelectors = map ((\x -> "column" ++ x ++ " as foocol" ++ x) . show) [1..nextCol-1]
        select = (embracket . intercalate " ") [ "select"
                                               , intercalate "," columnSelectors
                                               , "from"
                                               , valuesOfStringRows stringRows
                                               , "as foo" ]

embracket :: String -> String
embracket = ("("++) . (++")")
