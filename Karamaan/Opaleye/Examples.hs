{-# LANGUAGE Arrows #-}

module Karamaan.Opaleye.Examples where

import Karamaan.Opaleye.TableColspec (col)
import qualified Karamaan.Opaleye.Applicative as A
import Karamaan.Opaleye.Unpackspec (Unpackspec(Unpackspec))
import qualified Karamaan.Opaleye.Unpackspec as U
import Karamaan.Opaleye.QueryColspec (writerWire)
import Karamaan.Opaleye.Table (makeTable)
import Karamaan.Opaleye.QueryArr (Query, QueryArr)
import qualified Karamaan.Opaleye.Operators2 as Op2
import qualified Karamaan.Opaleye.Predicates as P
import qualified Karamaan.Opaleye.Operators.Numeric as N
import Karamaan.Opaleye.Wire (Wire)
import Karamaan.Opaleye.SQL (showSqlForPostgreSQLSimple)
import Control.Category ((<<<))
import Control.Arrow (arr, (&&&), returnA, (***))
import Data.Time.Calendar (Day)

personTable :: Query (Wire String, Wire Int, Wire String)
personTable = makeTable (A.a3 ( col "name"
                              , col "age"
                              , col "address" ))
                "personTable"

birthdayTable :: Query (Wire String, Wire Day)
birthdayTable = makeTable (A.a2 ( col "name"
                                , col "birthday" ))
                "birthdayTable"

nameAge :: Query (Wire String, Wire Int)
nameAge = arr (\(x, y, _) -> (x, y)) <<< personTable

personBirthdayProduct :: Query ((Wire String, Wire Int, Wire String),
                         (Wire String, Wire Day))
personBirthdayProduct = personTable &&& birthdayTable

totalAge :: Query (Wire String, Wire String, Wire Int)
totalAge = proc () -> do
  (name1, age1, _) <- personTable -< ()
  (name2, age2, _) <- personTable -< ()

  sumAge <- N.plus -< (age1, age2)

  returnA -< (name1, name2, sumAge)

personAndBirthday :: Query (Wire String, Wire Int, Wire String, Wire Day)
personAndBirthday = proc () -> do
  (name, age, address) <- personTable -< ()
  (name', birthday) <- birthdayTable -< ()

  P.restrict <<< Op2.eq -< (name, name')

  returnA -< (name, age, address, birthday)

birthdayOfPerson :: QueryArr (Wire String) (Wire Day)
birthdayOfPerson = proc name -> do
  (name', birthday) <- birthdayTable -< ()

  P.restrict <<< Op2.eq -< (name, name')

  returnA -< birthday

personAndBirthday' :: Query (Wire String, Wire Int, Wire String, Wire Day)
personAndBirthday' = proc () -> do
  (name, age, address) <- personTable -< ()
  birthday <- birthdayOfPerson -< name

  returnA -< (name, age, address, birthday)

children :: Query (Wire String, Wire Int, Wire String)
children = proc () -> do
  row@(_, age, _) <- personTable -< ()
  -- TODO: having to pull out the constant explicitly
  -- is a bit messy.  We need to come up with a better
  -- syntax for this.
  eighteen <- Op2.constant 18 -< ()
  P.restrict <<< N.lt -< (age, eighteen)

  returnA -< row

notTwentiesAtAddress :: Query (Wire String, Wire Int, Wire String)
notTwentiesAtAddress = proc () -> do
  row@(_, age, address) <- personTable -< ()
  twenty <- Op2.constant 20 -< ()
  thirty <- Op2.constant 30 -< ()

  P.restrict <<< Op2.or <<< (N.lt *** N.gte) -< ((age, twenty), (age, thirty))

  myAddress <- Op2.constant "1 My Street, My Town" -< ()

  P.restrict <<< Op2.eq -< (address, myAddress)

  returnA -< row

notTwentiesAtAddress' :: Query (Wire String, Wire Int, Wire String)
notTwentiesAtAddress' = proc () -> do
  row@(_, age, address) <- personTable -< ()
  twenty <- Op2.constant 20 -< ()
  thirty <- Op2.constant 30 -< ()

  P.restrict <<< Op2.or <<< (N.lt *** N.gte) -< ((age, twenty), (age, thirty))

  myAddress <- Op2.constant "1 My Street, My Town" -< ()

  P.restrict <<< Op2.eq -< (address, myAddress)

  returnA -< row

sql_personTable :: String
sql_personTable = s (U.pc3 (w, w, w)) personTable

sql_birthdayTable :: String
sql_birthdayTable = s (U.pc2 (w, w)) birthdayTable

sql_nameAge :: String
sql_nameAge = s (U.pc2 (w, w)) nameAge

sql_personBirthdayProduct :: String
sql_personBirthdayProduct = s unpackspec personBirthdayProduct
  where unpackspec = U.pc2 (U.pc3 (w, w, w), U.pc2 (w, w))

sql_totalAge :: String
sql_totalAge = s (U.pc3 (w, w, w)) totalAge

sql_personAndBirthday :: String
sql_personAndBirthday = s (U.pc4 (w, w, w, w)) personAndBirthday

sql_personAndBirthday' :: String
sql_personAndBirthday' = s (U.pc4 (w, w, w, w)) personAndBirthday'

sql_children :: String
sql_children = s (U.pc3 (w, w, w)) children

sql_notTwentiesAtAddress :: String
sql_notTwentiesAtAddress = s (U.pc3 (w, w, w)) notTwentiesAtAddress

sql_notTwentiesAtAddress' :: String
sql_notTwentiesAtAddress' = s (U.pc3 (w, w, w)) notTwentiesAtAddress'

w :: Unpackspec (Wire a)
w = Unpackspec writerWire

s :: Unpackspec a -> Query a -> String
s = showSqlForPostgreSQLSimple
