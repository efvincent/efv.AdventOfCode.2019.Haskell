module Utility where

import Data.Text (Text)
import Text.Read (readMaybe)

replace :: Int -> a -> [a] -> [a]
replace i e xs = case splitAt i xs of
    (before, _:after) -> before ++ e : after
    _ -> xs

stoi :: String -> Int
stoi = read

stoiM :: String -> Maybe Int
stoiM = readMaybe