module Day14 where

import           AdventData (day14,day14ex01,day14ex02,day14ex03,day14ex04,day14ex05)
import qualified Data.Map as M
import           Data.List.Split (splitOn)

rawFormulas :: String -> [[String]] 
rawFormulas raw =
    map (splitOn "=>") $ lines raw

f1 = ["7 A, 1 D "," 1 E"]
