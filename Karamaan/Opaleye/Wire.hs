module Karamaan.Opaleye.Wire where

newtype Wire a = Wire String deriving Show

unWire :: Wire a -> String
unWire (Wire a) = a

-- TODO: get rid of this one
unwire :: Wire a -> String
unwire = unWire

-- TODO: should this always be within a QueryArr?
-- If so, replace Join.unsafeCoerce with it
--
-- This would introduce a dependency on QueryArr, so we have
-- to think carefully about the module dependency structure.
unsafeCoerce :: Wire a -> Wire b
unsafeCoerce = Wire . unWire
