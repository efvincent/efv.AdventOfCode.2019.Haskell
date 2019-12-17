module Day02 where

import Data.List (splitAt)
import AdventData (day02)

replace :: Int -> a -> [a] -> [a]
replace i e xs = case splitAt i xs of
    (before, _:after) -> before ++ e : after
    _ -> xs

-- instructions say to replace posistions 1 and 2
initMem :: Int -> Int -> [Int]
initMem v1 v2 = replace 1 v1 (replace 2 v2 day02)

memory = initMem 12 2

run pos mem = 
    let 
        apply :: (Int -> Int -> Int) -> Int -> Int -> Int -> [Int] -> [Int]
        apply f p1 p2 pAns mem = replace pAns (f (mem !! p1) (mem !! p2)) mem in
    if op /= 99 then run (pos + 4) (apply fn p1 p2 pAns mem) else mem
    where
        (op:p1:p2:pAns:_) = drop pos mem
        fn = case op of
            1 -> (+)
            2 -> (*)
            99 -> \ _ _ -> 0

ans = head $ run 0 memory
