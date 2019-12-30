module Day10 where

import Data.List (sort, sortOn, group, groupBy, transpose)
import Data.Maybe (mapMaybe)
import AdventData (day10ex01,day10ex02,day10ex03,day10ex04,day10ex05,day10,day10ex06)

type Pos = (Int,Int)
type Ray2 = (Int, Float, Int, Pos)
type Vec = (Float, Pos)    -- this is a ray that includes the coordinate of the target node

-- | A "ray" describes the angle from -pi to pi, oriented so that N (up) is -pi, so that
-- when sorted by angle, the ones directly north will come first. After the angle is the
-- "Manhattan" distance, so we can order closest first
rayOf :: Pos -> Pos -> Maybe Ray2
rayOf (tx, ty) (x,y) | x == tx && y == ty = Nothing
rayOf (tx, ty) (x,y) =
    let angle = atan2 (fromIntegral (ty-y)) (fromIntegral (tx-x)) in
    Just (quadOf angle, angle, (abs (tx-x) + abs (ty-y)), (x,y))

halfPi = pi / 2.0
twoPi = pi * 2.0

quadOf :: Float -> Int
quadOf d | d >= halfPi  && d <= pi     = 1
quadOf d | d >= -pi     && d < -halfPi = 2
quadOf d | d >= -halfPi && d <= 0.0    = 3
quadOf d | d > 0        && d <= halfPi = 4

-- | Get list of cells from the raw input from the puzzle
cellsFromRaw :: String -> [Pos]
cellsFromRaw raw =
    concat $ loop 0 $ lines raw
  where
    -- | Gets the coordinates of cells with '#' from a string, using a fixed Y axis value
    cellsFromRawRow :: Int -> Int -> String -> [Pos]
    cellsFromRawRow _ _ [] = []
    cellsFromRawRow y x (c:rest) | c == '#' || c == 'X' = (x,y) : cellsFromRawRow y (x+1) rest
    cellsFromRawRow y x (_:rest) = cellsFromRawRow y (x+1) rest

    -- | loop over a set of lines
    loop :: Int -> [String] -> [[Pos]]
    loop _ [] = []
    loop y (row:rest) =
        cellsFromRawRow y 0 row : loop (y+1) rest

-- | Count number of cells in a list of cells visible from a given pos
visCount :: [Pos] -> Pos -> (Int,Pos)
visCount grid cell =
    let rays = mapMaybe (rayOf cell) grid in
    let len = length (groupBy grpPred $ sort rays) in
    (len,cell)

-- day10 = 326
solve :: String -> (Int, Pos)
solve raw =
    let g = cellsFromRaw raw in
    let counts = sort $ map (visCount g) g in
    counts !! max 0 (length counts - 1)

-- | interleaves list by taking the head of each element in the list repeatedly
-- and concatinating. Ex: [[1,2,3],[4,5],[6,7,8]] -> [1,4,6,2,5,7,3,8] 
interleaveLists :: [[a]] -> [a]
interleaveLists = concat . transpose

-- | extracts keys from the 4-tuple consisting of the tuple of the first 2
grpPred :: (Int,Float,a,b) -> (Int,Float,a,b) -> Bool
grpPred (q,a,_,_) (q',a',_,_) = (q,a) == (q',a')

-- | list of the asteroids to blast in the order they'll be blasted in
laserRays :: [Pos] -> Pos -> [Ray2]
laserRays grid cell =
    let rays = mapMaybe (rayOf cell) grid in
    let groups = groupBy grpPred $ sort rays in
    interleaveLists groups

-- day10 part 2 = 1623
solve2 :: String -> Int
solve2 raw =
    tx * 100 + ty
  where
    (c,pos) = solve raw
    g = cellsFromRaw raw
    lrays = laserRays g pos
    (_,_,_,(tx,ty)) = lrays !! 199  -- solution is the 200th element in the blast order list
          