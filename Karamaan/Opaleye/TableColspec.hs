{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Karamaan.Opaleye.TableColspec where

import Karamaan.Opaleye.Wire (Wire(Wire))
import Karamaan.Opaleye.QueryColspec (QueryColspec, runWriterOfQueryColspec,
                                     runPackMapOfQueryColspec)
import Control.Applicative (Applicative, pure, (<*>))
import Data.Monoid (Monoid, mempty)

data TableColspec a = TableColspec [String] ((String -> String) -> a)

tableColspecOfQueryColspec :: QueryColspec a b -> a -> TableColspec b
tableColspecOfQueryColspec q a = TableColspec (runWriterOfQueryColspec q a)
                                          (\b -> runPackMapOfQueryColspec q b a)

instance Functor TableColspec where
  fmap f (TableColspec s m) = TableColspec s (f . m)

instance Applicative TableColspec where
  pure = TableColspec mempty . pure
  TableColspec s mf <*> TableColspec s' m = TableColspec (s ++ s') (mf <*> m)

runWriterOfColspec :: TableColspec a -> [String]
runWriterOfColspec (TableColspec s _) = s

runPackMapOfColspec :: TableColspec a -> (String -> String) -> a
runPackMapOfColspec (TableColspec _ m) = m

colspec :: [String] -> ((String -> String) -> a) -> TableColspec a
colspec = TableColspec

col :: String -> TableColspec (Wire a)
col s = colspec [s] (\f -> Wire (f s))
