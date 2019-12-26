-- Needed so that `M.Map String String` can be used instead of `M.Map [Char] [Char]`
{-# LANGUAGE OverloadedStrings #-}

module Day06b where

import           Data.List.Split (splitOn)
import           Data.List ((\\))
import           Control.Monad (foldM)
import qualified Data.Map.Strict as M
import           AdventData (day06,day06ex01,day06ex02)

type OrbitMap = M.Map String [String]

makeOrbitMap :: String -> OrbitMap
makeOrbitMap raw =
    fullMap
  where
    pairs = map (splitOn [')']) $ lines raw :: [[String]]
    dict = M.fromList . map (\(e1:e2:xs) -> (e2, [e1])) $ pairs :: OrbitMap

    updateMapEntry :: String -> [String] -> [String]
    updateMapEntry newValue edges =
        if newValue `notElem` edges
        then newValue : edges
        else edges

    addLink :: OrbitMap -> [String] -> OrbitMap
    addLink m [key,newLink] =
        case M.lookup key m of
            Just edges -> M.insert key (updateMapEntry newLink edges) m
            Nothing -> M.insert key [newLink] m
    fullMap = foldl addLink dict pairs

type Planet = String
type Path = [Planet]
type Dead = [Planet]
data State = State { path :: Path
                   , dead :: Dead
                   , to   :: Planet
                   , om   :: OrbitMap
                   } deriving (Show)

filterM :: [Maybe a] -> [a]
filterM [] = []
filterM (Nothing : xs) = filterM xs
filterM (Just v : xs) = v : filterM xs

-- | [Candidates] ->
solve1 :: [Planet] -> State -> Maybe State
solve1 candidates state =
    -- remove everything in the (path state) and (dead state) from candidates
    -- if left with [] then Nothing
    -- else foldM candidates over solve2
    ans
  where
    cands = candidates \\ (path state ++ dead state)
    ans =
        case cands of
            [] -> Nothing
            cands ->
                case filterM $ map (solve2 state) cands of
                    (s:ss) -> Just s
                    [] -> Nothing

solve2 :: State -> Planet -> Maybe State
solve2 state current =
    -- If the current candidate == (to state) then Just (path state)

    -- Append the current location to the path
    -- For the current location, get all the candidates
    -- if there are no candidates, return Nothing
    -- otherwise, solve1 state cadidates

    if current == to state
    then Just state
    else
        newState
      where
        newp = current : path state
        newState = case M.lookup current (om state) of
            Just cands' | null cands' -> Nothing
            Just cands' -> solve1 cands' state { path = newp }
            Nothing -> Nothing

{-
The following finds a solution, but not the shortest solution
-}
s = State { path = [], dead = [], to = "SAN", om = makeOrbitMap day06 }
ja = solve1 ["YOU"] s
(Just a) = ja
ans = length (path a) - 1        -- -1 for "YOU" in the path