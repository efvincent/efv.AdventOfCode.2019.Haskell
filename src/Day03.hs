module Day03 where

import AdventData (day03)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Utility

type Loc = (Int, Int)

type Intersection = (Loc, S.Set Int)

type Instruction = (Char, Int)

toInstruction :: String -> Instruction
toInstruction s = (head s, stoi (tail s))

instToPath :: [Instruction] -> [(Loc, Int)]
instToPath ins = loop ins ((0, 0), 0)
  where
    loop [] _ = []
    -- These loop through the instructions, for each it generats a list `steps` long, where the coordinate X or Y
    -- is inc(dev)remented as appropriate, also accumulating the total steps `t` along the way. This generates
    -- a lot of data since each line segment has a (Loc,Int), but that's needed so that we can find intersection
    -- with the next line
    loop (('U', steps):xs) ((x, y), t) =
        [((x, y + step), t + step) | step <- [1 .. steps]] ++ loop xs ((x, y + steps), t + steps)
    loop (('D', steps):xs) ((x, y), t) =
        [((x, y - step), t + step) | step <- [1 .. steps]] ++ loop xs ((x, y - steps), t + steps)
    loop (('L', steps):xs) ((x, y), t) =
        [((x - step, y), t + step) | step <- [1 .. steps]] ++ loop xs ((x - steps, y), t + steps)
    loop (('R', steps):xs) ((x, y), t) =
        [((x + step, y), t + step) | step <- [1 .. steps]] ++ loop xs ((x + steps, y), t + steps)

instructions :: String -> [[Instruction]]
instructions raw = map (map toInstruction . splitOn ",") (lines raw)

findPaths :: String -> [[(Loc, Int)]]
findPaths raw = map instToPath (instructions raw)

-- | given the raw, p1 and p2 are the paths as [Loc]. Make a set from locations occupied by p1,
-- filter the p2 locations to only those cells in the set of p1 locations. Find the manhattan
-- distances, then find the lowes in that set.
findIntersections raw =
    "intersections: " ++
    show ints ++
    "\n" ++ "Manhattan distances: " ++ show mds ++ "\n" ++ "minimum: " ++ show closest ++ "\n"
  where
    [p1, p2] = findPaths raw
    board = S.fromList $ map fst p1
    ints = filter (`S.member` board) $ map fst p2
    mds = map (\(x, y) -> abs x + abs y) ints
    closest = minimum mds

findSigDelay raw = "Minimum combined lengths: " ++ show minCombLens ++ "\n"
  where
    [p1, p2] = findPaths raw
    board = M.fromList p1
    ints = [((x, y), t2) | ((x, y), t2) <- p2, M.member (x, y) board] -- gets you the intersections and p2 distances
    combinedLengths =
        map
            (\(loc, t2) ->
                 let t1 = board M.! loc
                  in t1 + t2)
            ints
    minCombLens = minimum combinedLengths
