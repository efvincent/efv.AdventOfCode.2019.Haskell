module Day06a where

import           AdventData      (day06, day06ex01)
import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

tokv :: [a] -> (a, a) -- make key value pair out of a 2 element list, 2nd element is the key
tokv (e1 : e2 : xs) = (e2, e1)

d = M.fromList . map (tokv . splitOn [')']) $ lines day06

d' = M.fromList . map (tokv . splitOn [')']) $ lines day06ex01

ans = sum . map (countChain d) $ M.keys d

ans' = sum . map (countChain d') $ M.keys d'

countChain :: M.Map String String -> String -> Int
countChain dict = loop 0
  where
    loop :: Int -> String -> Int
    loop acc k = case M.lookup k dict of
        Just next -> loop (acc + 1) next
        Nothing   -> acc
