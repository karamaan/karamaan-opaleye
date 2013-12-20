{-# LANGUAGE Arrows, FlexibleContexts #-}
-- ^^ Get rid of FlexibleContexts if we ever move the definition of s
--    elsewhere.
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Karamaan.Opaleye.Examples where

import Karamaan.Opaleye.Unpackspec (Unpackspec)
import Karamaan.Opaleye.Table (makeTableDef)
import Karamaan.Opaleye.QueryArr (Query, QueryArr)
import qualified Karamaan.Opaleye.Operators2 as Op2
import qualified Karamaan.Opaleye.Predicates as P
import qualified Karamaan.Opaleye.Operators.Numeric as N
import Karamaan.Opaleye.Wire (Wire)
import Karamaan.Opaleye.SQL (showSqlForPostgresDefault)
import Control.Category ((<<<))
import Control.Arrow (arr, (&&&), returnA, (***))
import Data.Time.Calendar (Day)
import Data.Profunctor.Product.Default (Default)
import Data.Profunctor.Product (PPOfContravariant)

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Profunctor.Product (ProductProfunctor, p2)
import Data.Profunctor (dimap)
import Data.Profunctor.Product.Default (Default, def)

personTable :: Query (Wire String, Wire Int, Wire String)
personTable = makeTableDef ("name", "age", "address") "personTable"

data Birthday' a b = Birthday { bdName :: a, bdDay :: b }
type Birthday = Birthday' String Day
type WireBirthday = Birthday' (Wire String) (Wire Day)

$(makeAdaptorAndInstance "pBirthday" ''Birthday')

birthdayTable :: Query WireBirthday
birthdayTable = makeTableDef (Birthday { bdName = "name"
                                       , bdDay = "birthday" })
                    "birthdayTable"

nameAge :: Query (Wire String, Wire Int)
nameAge = arr (\(x, y, _) -> (x, y)) <<< personTable

personBirthdayProduct :: Query ((Wire String, Wire Int, Wire String),
                                WireBirthday)
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
  birthday <- birthdayTable -< ()

  P.restrict <<< Op2.eq -< (name, bdName birthday)

  returnA -< (name, age, address, bdDay birthday)

birthdayOfPerson :: QueryArr (Wire String) (Wire Day)
birthdayOfPerson = proc name -> do
  birthday <- birthdayTable -< ()

  P.restrict <<< Op2.eq -< (name, bdName birthday)

  returnA -< bdDay birthday

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
sql_personTable = s personTable

sql_birthdayTable :: String
sql_birthdayTable = s birthdayTable

sql_nameAge :: String
sql_nameAge = s nameAge

sql_personBirthdayProduct :: String
sql_personBirthdayProduct = s personBirthdayProduct

sql_totalAge :: String
sql_totalAge = s totalAge

sql_personAndBirthday :: String
sql_personAndBirthday = s personAndBirthday

sql_personAndBirthday' :: String
sql_personAndBirthday' = s personAndBirthday'

sql_children :: String
sql_children = s children

sql_notTwentiesAtAddress :: String
sql_notTwentiesAtAddress = s notTwentiesAtAddress

sql_notTwentiesAtAddress' :: String
sql_notTwentiesAtAddress' = s notTwentiesAtAddress'

s :: Default (PPOfContravariant Unpackspec) a a
                             => Query a
                             -> String
s = showSqlForPostgresDefault
