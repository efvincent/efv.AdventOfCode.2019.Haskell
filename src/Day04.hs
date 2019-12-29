module Day04 where

import Data.List (group)

vmin = 284639
vmax = 748759

-- | Turns an integer into an list of digits
digs :: Int -> [Int]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

-- | Assures that each digit in a list is equal or greater than the previous
fixup :: [Int] -> [Int]
fixup [] = []
fixup [n] = [n]
fixup (n1:n2:n3:rest) | n2 < n1 = n1 : fixup (n1:n1:rest)
fixup [n1,n2] | n2 < n1 = [n1,n1] 
fixup (n1:rest) = n1 : fixup rest

-- | turns a list of digits back into a number
undigs :: [Int] -> Int
undigs [] = 0
undigs (n:xs) = n * (10 ^ length xs) + undigs xs

-- | first solution, rising sequnce of digits (via fixup), valid has at least one
-- occurence of at least 2 sequential digits
proc1 :: Int -> Int -> [Int] -> [Int]
proc1 mx n c
    | n' > mx = c
    | isValid = proc1 mx (n'+1) (n':c)
    | otherwise = proc1 mx (n'+1) c
  where
    check = fixup $ digs n
    n' = undigs check
    isValid = (maximum . map length . group $ check) >= 2

-- | second solution, same as first, except no sequences of equal digits longer than 2
proc2 :: Int -> Int -> [Int] -> [Int]
proc2 mx n c
    | n' > mx = c
    | isValid = proc2 mx (n'+1) (n':c)
    | otherwise = proc2 mx (n'+1) c
  where
    check = fixup $ digs n
    n' = undigs check
    isValid = elem 2 . map length . group $ check

solution1 = length $ proc1 vmax vmin []
solution2 = length $ proc2 vmax vmin []