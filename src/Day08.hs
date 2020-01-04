module Day08 where

import AdventData (day08)
import Data.Char (digitToInt)
import qualified Data.List.Key as DLK
import Data.List.Split (chunksOf)

makeRows :: Int -> String -> [String]
makeRows w s =
    if null s
        then []
        else take w s : makeRows w (drop w s)

solve1 :: Int -> Int -> String -> Int
solve1 w h raw = ones * twos
  where
    stats :: String -> (Int, Int, Int)
    stats ns =
        ( length . filter (== ' ') $ ns
        , length . filter (== '1') $ ns
        , length . filter (== '2') $ ns)
    s = map stats $ makeRows (w * h) raw
    (zeros, ones, twos) = DLK.minimum (\(zeros, _, _) -> zeros) s

resolve :: String -> Char
resolve [] = error "No non-zero elements in list"
resolve (c:cs)
    | c == '2' = resolve cs
resolve (c:_)
    | c == '0' = ' '
resolve (c:_)
    | c == '1' = '#'

-- plan for solve 2 - make rows where each row is a layer
-- make a new list where each position is the same index from each of the layers
solve2 w h raw = chunksOf w $ map resolve stacked
  where
    layers = makeRows (w * h) raw -- 1   of these layers. Each has 25 * 6 = 15  pixel elements, 6 rows of 25 pixels
    stacked = map (\n -> map (!! n) layers) [0 .. ((w * h) - 1)]

printSolved :: [String] -> IO ()
printSolved (x:xs) = do
    print s
    printSolved xs
  where
    s = "     " ++ x ++ "     "
printSolved [] = print "Done!"
