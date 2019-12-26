-- Needed so that `M.Map String String` can be used instead of `M.Map [Char] [Char]`
{-# LANGUAGE OverloadedStrings #-}

module Day06b where

import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import           AdventData (day06,day06ex01)

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



    -- M.fromList . map (toMapOfList . splitOn [')']) $ lines day06ex01