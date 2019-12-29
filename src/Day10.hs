module Day10 where

import Data.Ratio
import Data.List (sort, group)
import AdventData (day10ex01,day10ex02,day10ex03,day10ex04,day10ex05,day10)

type IRatio = Ratio Int

-- | Indicates the quadrant of a cell relative to a target cell. Qn is quadrant number starting at 1 (top left) 
--   going clockwise. Right and Left indicate cells to the right and left of the target cell, for which
--   a ratio cannot be calculated due to zero denominiator. Since this is effectively a ray out the left and 
--   right, we don't need a ratio, just the direction
data Ray = Q1 IRatio | Q2 IRatio | Q3 IRatio | Q4 IRatio
         | R | L | Equal
         deriving (Show, Eq, Ord)

type Pos = (Int,Int)
type Grid = [Pos]

ratio :: Pos -> Pos -> IRatio
ratio (tx,ty) (x,y) = (x - tx) % (y - ty)

-- | Determine the quadrant and ratio of a target cell
rayOf :: Pos -> Pos -> Ray
rayOf (tx, ty) (x,y) | x == tx && y == ty = Equal
rayOf (tx, ty) (x,y) | x <  tx && y == ty = L
rayOf (tx, ty) (x,y) | x >  tx && y == ty = R
rayOf (tx, ty) (x,y) | x <= tx && y <  ty = Q1 ((tx,ty) `ratio` (x,y))
rayOf (tx, ty) (x,y) | x >  tx && y <  ty = Q2 ((tx,ty) `ratio` (x,y))
rayOf (tx, ty) (x,y) | x >  tx && y >  ty = Q3 ((tx,ty) `ratio` (x,y))
rayOf (tx, ty) (x,y) | x <= tx && y >  ty = Q4 ((tx,ty) `ratio` (x,y))

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

-- | Count number of cells in a grid visible from a given pos
visCount :: Grid -> Pos -> Int
visCount grid cell =
    let rays = map (rayOf cell) grid in
    length (group . sort $ rays) - 1

solve :: String -> Int
solve raw =
    let g = cellsFromRaw raw in
    maximum $ map (visCount g) g
