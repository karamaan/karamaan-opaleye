{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Karamaan.Opaleye.TableColspec where

import Karamaan.Opaleye.Wire (Wire(Wire))
import Control.Applicative (Applicative, pure, (<*>))
import Data.Monoid (Monoid, mempty)

-- This seems to be basically a Colspec' with the column names already applied
data TableColspec a = TableColspec [String] ((String -> String) -> a)

instance Functor TableColspec where
  fmap f (TableColspec s m) = TableColspec s (f . m)

instance Applicative TableColspec where
  pure = TableColspec mempty . pure
  TableColspec s mf <*> TableColspec s' m = TableColspec (s ++ s') (mf <*> m)

runWriterOfColspec :: TableColspec a -> [String]
runWriterOfColspec (TableColspec s _) = s

runPackMapOfColspec :: TableColspec a -> (String -> String) -> a
runPackMapOfColspec (TableColspec _ m) f = m f

colspec :: [String] -> ((String -> String) -> a) -> TableColspec a
colspec s m = TableColspec s m

col :: String -> TableColspec (Wire a)
col s = colspec [s] (\f -> Wire (f s))
