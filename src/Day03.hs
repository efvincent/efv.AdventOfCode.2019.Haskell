{-# LANGUAGE OverloadedStrings #-}
module Day03 where

import Data.Map.Strict (empty, insert)
import Data.List.Split (splitOn)
import Data.Bifunctor (second)
import qualified Data.Set as S
import AdventData (day03)

type Loc = (Int,Int)
type Intersection = (Loc, S.Set Int)
type Instruction = (Dir, Int)

data Dir = R | U | L | D deriving (Show)

stoi :: String -> Int
stoi s = read s :: Int

dirFromChar :: Char -> Dir
dirFromChar c
  = case c of 'U' -> U; 'D' -> D; 'L' -> L; 'R' -> R
              _ -> error "Bad instruction"

toInstruction :: String -> Instruction
toInstruction s = (dirFromChar (head s), stoi (tail s))

instToPath :: [Instruction] -> [Loc]
instToPath ins =
  [(0,0)] ++ loop ins (0,0)
  where
    loop [] _ = []
    loop ((U,n):xs) (x,y) = [(x, y+y') | y' <- [1..n]] ++ loop xs (x,y+n)
    loop ((D,n):xs) (x,y) = [(x, y-y') | y' <- [1..n]] ++ loop xs (x,y-n)
    loop ((L,n):xs) (x,y) = [(x-x', y) | x' <- [1..n]] ++ loop xs (x-n,y)
    loop ((R,n):xs) (x,y) = [(x+x', y) | x' <- [1..n]] ++ loop xs (x+n,y)

instructions rawData = map (map toInstruction . splitOn ",") (lines rawData)

findPaths rawData = zip [0..] $ map instToPath (instructions rawData)

pathLens :: [(Int,[Loc])] -> [(Int,Int)]
pathLens = map (second length)