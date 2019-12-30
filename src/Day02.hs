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

run :: Int -> [Int] -> [Int]
run pos mem =
    if op /= 99 
    then run (pos + 4) (replace pAns (fn (mem!!p1) (mem!!p2)) mem) 
    else mem
    where
        (op:p1:p2:pAns:_) = drop pos mem
        fn = case op of 1 -> (+); 2 -> (*); 99 -> \ _ _ -> 0

day2part1 = head $ run 0 memory

-- Part 2, run through combinations of initialized memory looking for a final value that
-- satisfies the formula given
target = 19690720

-- | Given a range as [(Int,Int)] and a target, find the pair that solves or nothing
solve :: [(Int,Int)] -> Int -> Maybe (Int,Int)
solve [] _ = Nothing
solve ((v1,v2):xs) target =
    if target == head (run 0 $ initMem v1 v2)
    then Just (v1, v2)
    else solve xs target

-- | This can be simplified (pretty sure) once I get to the point of learning
-- how to use `do` notation and the `Maybe` monad
day2part2 = case solve [(x,y) | x <- [0..99], y <- [0..99]] target of
            Just (v1, v2) -> Just (100 * v1 + v2)
            Nothing -> Nothing
