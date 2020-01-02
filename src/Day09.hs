module Day09 where

import qualified IntCode as IC
import AdventData (day09ex01,day09ex02,day09ex03,day09)
import Day05 (stepThru)

wex01 = IC.initialWorld { IC.input = Nothing, IC.mem = day09ex01 }
wex02 = wex01 { IC.mem = day09ex02 }
wex03 = wex01 { IC.mem = day09ex03 }

solution1 :: IO ()
solution1 = do
    let w = IC.initialWorld  { IC.input = Just 1, IC.mem = day09 }
    stepThru w

solution2 :: IO ()
solution2 = do
    let w = IC.initialWorld  { IC.input = Just 2, IC.mem = day09 }
    stepThru w