module Day06b where

import           AdventData      (day06, day06ex01, day06ex02)
import           Data.List       (sortOn, (\\))
import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import           Data.Maybe      (mapMaybe)

type OrbitMap = M.Map String [String]

makeOrbitMap :: String -> OrbitMap
makeOrbitMap raw = foldl addLink dict pairs
  where
    pairs = map (splitOn [')']) $ lines raw :: [[String]]
    dict  = M.fromList . map (\(e1 : e2 : xs) -> (e2, [e1])) $ pairs :: OrbitMap
    updateMapEntry :: String -> [String] -> [String]
    updateMapEntry newValue edges = if newValue `notElem` edges then newValue : edges else edges
    addLink :: OrbitMap -> [String] -> OrbitMap
    addLink m [key, newLink] = case M.lookup key m of
        Just edges -> M.insert key (updateMapEntry newLink edges) m
        Nothing    -> M.insert key [newLink] m

type Planet = String

data State =
    State
        { path :: [Planet]
        , dead :: [Planet]
        , to   :: Planet
        , om   :: OrbitMap
        }
    deriving (Show)

checkCandidates :: [Planet] -> State -> Maybe State
checkCandidates candidates state = case candidates \\ (path state ++ dead state) of
    []    -> Nothing
    cands -> case mapMaybe (findCandidates state) cands of
        []     -> Nothing
        states -> Just $ head (sortOn path states)
    -- remove everything already in path or dead from candidates
    -- if left with [] then Nothing
    -- else foldM candidates over solve2

-- | Assuming the current planet will be part of the solution, find the candidates
-- for the next step. If the current state is the target state, then we're done
findCandidates :: State -> Planet -> Maybe State
findCandidates state current = case (current == to state, M.lookup current (om state)) of
    (True, _)              -> Just state
    (_, Just cs) | null cs -> Nothing
    (_, Just cs)           -> checkCandidates cs state { path = current : path state }
    (_, Nothing)           -> Nothing
    -- Append the current planet to the path
    -- For the current planet, get all possible next steps
    -- if there are no possible next steps, this is a failed path, return Nothing
    -- otherwise, use checkCandidates on the list of all possible next steps

-- | Given the raw orbit map, the from planet and the two planet, find the
-- length of the shortest trip
solve :: String -> String -> String -> Int
solve raw from to =
    let s = State { path = [], dead = [], to = to, om = makeOrbitMap raw } in
    let (Just a) = checkCandidates [from] s in
    length (path a) - 2 -- -2 b/c we discount the actual start and destination
