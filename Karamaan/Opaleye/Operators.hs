module Karamaan.Opaleye.Operators where

-- TODO vv I put this take 5 in here because the query strings were getting
-- too long and postgres was complaining that it was truncating them.
-- This is really just a temporary fix, because I'd like to keep the
-- possibility of long names but postprocess the PrimQuery to shorten
-- them before sending them to postgres.
operatorName :: String -> String -> String -> String
operatorName left opName right = concat [t left, "_", opName, "_", t right]
  where t = take 5
