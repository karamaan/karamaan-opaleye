module Karamaan.Opaleye.Wire where

newtype Wire a = Wire String deriving Show

unWire :: Wire a -> String
unWire (Wire a) = a

-- TODO: get rid of this one
unwire :: Wire a -> String
unwire = unWire

-- TODO: should this always be within a Query?
-- If so, replace Join.unsafeCoerce with it
unsafeCoerce :: Wire a -> Wire b
unsafeCoerce = Wire . unWire
