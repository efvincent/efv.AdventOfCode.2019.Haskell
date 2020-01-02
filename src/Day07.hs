module Day07 where

import           AdventData (day07,day07ex01,day07ex02,day07ex03,day07bex01,day07bex02)
import           Data.List (permutations)
import qualified IntCode as IC

progSets :: [Integer] -> Integer -> Integer -> [[IC.World]]
progSets mem n1 n2 =
    map makeProgSet $ permutations [n1..n2]
  where
    makeProgSet :: [Integer] -> [IC.World]
    makeProgSet settings =
      let idxSettings = zip [0..] settings in 
      map makeWorld idxSettings

    makeWorld :: (Int, Integer) -> IC.World
    makeWorld (wid, w) =
        IC.World { IC.input = Just w
                 , IC.output = Nothing
                 , IC.mem = mem
                 , IC.inWait = False
                 , IC.wid = wid
                 , IC.rbase = 0
                 , IC.offset = 0 }

runSeries :: Maybe Integer -> [IC.World] -> Maybe Integer
runSeries sig (w:ws) =
    case ws of
        [] -> res
        remaining -> runSeries res remaining
  where
    res = case sig of
      Just s ->  IC.getOutput $ IC.run (IC.setInput s w)
      Nothing -> error "no signal to pass on"

ansEx01 = maximum $ map (runSeries 0) (progSets day07ex01 0 4)
ansEx02 = maximum $ map (runSeries 0) (progSets day07ex02 0 4)
ansEx03 = maximum $ map (runSeries 0) (progSets day07ex03 0 4)
ans     = maximum $ map (runSeries 0) (progSets day07 0 4)

-- for part 2, we kick off with the initial inputs, connect the outputs to the inputs,
-- and connect the last output to the input of the first, and run it in a loop until you
-- get a non inWait termination. That output is the answer

-- run one progset
runProgSet :: [IC.World] -> Integer
runProgSet = loop Nothing
  where
    loop :: Maybe IC.World -> [IC.World] -> Integer
    loop (Just prev) (next:rest) =
        -- In the case where there was a pervious run, use it's output
        -- as the input to the current one. Also, push the last one at
        -- the end of the queue
        if resWaiting || ((IC.wid result') /= 4)
        then loop (Just result') rest'
        else head $ IC.output result
      where
        next' = next { IC.input = (IC.input next) ++ IC.output prev } -- Feed next with out of prev
        prev' = prev { IC.output = [] }                 -- Clear out the prev's output
        rest' = rest ++ [prev']                      -- Put the prev at the end of the queue
        result = IC.run next'
        resWaiting = IC.inWait result
        result' = result { IC.inWait = False }
    loop Nothing (first:rest) =
        -- First time through the loop there is no previous, we'll seed and run the head of the list
        -- with a zero signal, and continue the loop 
        loop (Just result) rest
      where
        first' = first { IC.input = IC.input first ++ [0] }
        result = IC.run first' { IC.inWait = False }

solve :: [Integer] -> Integer
solve prog =
    maximum ans
  where
    pss = progSets prog 5 9
    ans = map runProgSet pss