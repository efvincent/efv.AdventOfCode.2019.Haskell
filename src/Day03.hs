module Day03 where

import Data.Map.Strict (empty, insert)
import Data.List.Split (splitOn)
import AdventData (day03)

instructions = map (splitOn ",") (lines day03)

