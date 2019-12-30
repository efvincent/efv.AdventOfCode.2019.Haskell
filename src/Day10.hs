module Day10 where

import Data.List (sort, sortOn, group, groupBy)
import Data.Maybe (mapMaybe)
import AdventData (day10ex01,day10ex02,day10ex03,day10ex04,day10ex05,day10)

type Pos = (Int,Int)
type Ray2 = (Int, Float, Int, Pos)
type Vec = (Float, Pos)    -- this is a ray that includes the coordinate of the target node

-- | A "ray" describes the angle from -pi to pi, oriented so that N (up) is -pi, so that
--   when sorted by angle, the ones directly north will come first. After the angle is the
--   "Manhattan" distance, so we can order closest first
rayOf :: Pos -> Pos -> Maybe Ray2
rayOf (tx, ty) (x,y) | x == tx && y == ty = Nothing
rayOf (tx, ty) (x,y) =
    let angle = atan2 (fromIntegral (ty-y)) (fromIntegral (tx-x)) in
    Just (quadOf angle, angle, (abs (tx+x) + abs (ty+y)), (x,y))

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
    cellsFromRawRow y x (c:rest) | c == '#' = (x,y) : cellsFromRawRow y (x+1) rest
    cellsFromRawRow y x (_:rest) = cellsFromRawRow y (x+1) rest

    -- | loop over a set of lines
    loop :: Int -> [String] -> [[Pos]]
    loop _ [] = []
    loop y (row:rest) =
        cellsFromRawRow y 0 row : loop (y+1) rest

-- | Count number of cells in a list of cells visible from a given pos
visCount :: [Pos] -> Pos -> Int
visCount grid cell =
    let rays = map (\(q,a,md,p) -> (q,a)) $ mapMaybe (rayOf cell) grid in
    length (group . sort $ rays)

laserRays :: [Pos] -> Pos -> [[Ray2]]
laserRays grid cell =
    let rays = mapMaybe (rayOf cell) grid in
    groupBy (\(q,a,_,_) (q',a',_,_) -> (q,a) == (q',a')) $ sort rays

-- day10 = 326
solve :: String -> Int
solve raw =
    let g = cellsFromRaw raw in
    maximum $ map (visCount g) g
