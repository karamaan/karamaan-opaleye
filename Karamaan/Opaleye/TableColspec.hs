{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Karamaan.Opaleye.TableColspec where

import Karamaan.Opaleye.Wire (Wire(Wire))
import Karamaan.Opaleye.QueryColspec (QueryColspec(QueryColspec),
                                      runWriterOfQueryColspec,
                                      runPackMapOfQueryColspec,
                                      MWriter(Writer),
                                      PackMap(PackMap))
import Control.Applicative (Applicative, pure, (<*>))
import Data.Monoid (Monoid, mempty)
import Karamaan.Opaleye.Default (Default, def)

-- TODO: this happens to have the same implementation as QueryColspec, but I
-- don't want to suggest that one derives from the other.  Perhaps make this
-- clearer by introducing another type from which they both inherit their
-- implementation.
newtype TableColspecP a b = TableColspecP (QueryColspec a b)

data TableColspec a = TableColspec [String] ((String -> String) -> a)

tableColspecOfTableColspecP :: TableColspecP a b -> a -> TableColspec b
tableColspecOfTableColspecP (TableColspecP q) a =
  TableColspec (runWriterOfQueryColspec q a)
               (\b -> runPackMapOfQueryColspec q b a)

instance Functor TableColspec where
  fmap f (TableColspec s m) = TableColspec s (f . m)

instance Applicative TableColspec where
  pure = TableColspec mempty . pure
  TableColspec s mf <*> TableColspec s' m = TableColspec (s ++ s') (mf <*> m)

instance Default TableColspecP String (Wire a) where
  def = TableColspecP (QueryColspec (Writer return) (PackMap (Wire .)))

runWriterOfColspec :: TableColspec a -> [String]
runWriterOfColspec (TableColspec s _) = s

runPackMapOfColspec :: TableColspec a -> (String -> String) -> a
runPackMapOfColspec (TableColspec _ m) = m

colspec :: [String] -> ((String -> String) -> a) -> TableColspec a
colspec = TableColspec

col :: String -> TableColspec (Wire a)
col s = colspec [s] (\f -> Wire (f s))
