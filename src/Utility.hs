module Utility where

replace :: Int -> a -> [a] -> [a]
replace i e xs = case splitAt i xs of
    (before, _:after) -> before ++ e : after
    _ -> xs