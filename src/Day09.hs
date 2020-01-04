module Day09 where

import           AdventData (day09, day09ex01, day09ex02, day09ex03)
import           IntCode

wex01 = setMemory initialComputer day09ex01

wex02 = setMemory wex01 day09ex02

wex03 = setMemory wex01 day09ex03

solution1 :: IO ()
solution1 = do
    let w = setInput (setMemory initialComputer day09) 1
    stepThru w

solution2 :: IO ()
solution2 = do
    let w = setInput (setMemory initialComputer day09) 2
    stepThru w
