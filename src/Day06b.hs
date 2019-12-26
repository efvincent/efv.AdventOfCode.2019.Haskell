module Day06b where

import           Data.List.Split (splitOn)
import           Data.List ((\\),sortOn)
import           Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as M
import           AdventData (day06,day06ex01,day06ex02)

type OrbitMap = M.Map String [String]

makeOrbitMap :: String -> OrbitMap
makeOrbitMap raw =
    foldl addLink dict pairs
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


type Planet = String
data State = State { path :: [Planet]
                   , dead :: [Planet]
                   , to   :: Planet
                   , om   :: OrbitMap
                   } deriving (Show)

checkCandidates :: [Planet] -> State -> Maybe State
checkCandidates candidates state =
    -- remove everything already in path or dead from candidates
    -- if left with [] then Nothing
    -- else foldM candidates over solve2
    case candidates \\ (path state ++ dead state) of
        [] -> Nothing
        cands ->
            case mapMaybe (solve2 state) cands of
                [] -> Nothing
                states -> Just $ head (sortOn path states)

solve2 :: State -> Planet -> Maybe State
solve2 state current =
    -- If the current candidate == (to state) then Just (path state)
    -- Append the current location to the path
    -- For the current location, get all the candidates
    -- if there are no candidates, return Nothing
    -- otherwise, checkCandidates state cadidates
    case (current == to state, M.lookup current (om state)) of
    (True,_) -> Just state
    (_,Just cands') | null cands' -> Nothing
    (_,Just cands') -> checkCandidates cands' state { path = current : path state }
    (_,Nothing) -> Nothing

solve :: String -> String -> String -> Int
solve raw from to =
    let s = State { path = [], dead = [], to = to, om = makeOrbitMap raw } in
    let ja = checkCandidates [from] s in
    let (Just a) = ja in
    length (path a) - 2        -- -2 b/c we discount the actual start and destination