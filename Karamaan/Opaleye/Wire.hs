module Karamaan.Opaleye.Wire where

newtype Wire a = Wire String deriving Show

unWire :: Wire a -> String
unWire (Wire a) = a

-- TODO: get rid of this one
unwire :: Wire a -> String
unwire = unWire

unsafeCoerce :: Wire a -> Wire b
unsafeCoerce = Wire . unWire
