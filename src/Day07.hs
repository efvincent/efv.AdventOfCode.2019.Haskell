module Day07 where

import           AdventData    (day07, day07bex01, day07bex02, day07ex01,
                                day07ex02, day07ex03)
import           Data.List     (permutations)
import qualified Day05Original as D

progSets :: [Integer] -> Integer -> Integer -> [[D.World]]
progSets mem n1 n2 = map makeProgSet $ permutations [n1 .. n2]
  where
    makeProgSet :: [Integer] -> [D.World]
    makeProgSet settings = let idxSettings = zip [0 ..] settings in map makeWorld idxSettings
    makeWorld :: (Int, Integer) -> D.World
    makeWorld (wid, w) = D.World { D.ins    = [w]
                                 , D.outs   = []
                                 , D.mem    = mem
                                 , D.inWait = False
                                 , D.wid    = wid
                                 , D.rbase  = 0
                                 , D.offset = 0
                                 }

runSeries :: Integer -> [D.World] -> Integer
runSeries sig (w : ws) = case ws of
    []        -> res
    remaining -> runSeries res remaining
    where res = head $ D.outs $ D.run w { D.ins = D.ins w ++ [sig] }

ansEx01 = maximum $ map (runSeries 0) (progSets day07ex01 0 4)

ansEx02 = maximum $ map (runSeries 0) (progSets day07ex02 0 4)

ansEx03 = maximum $ map (runSeries 0) (progSets day07ex03 0 4)

ans = maximum $ map (runSeries 0) (progSets day07 0 4)

-- for part 2, we kick off with the initial inputs, connect the outputs to the inputs,
-- and connect the last output to the input of the first, and run it in a loop until you
-- get a non inWait termination. That output is the answer
-- run one progset
runProgSet :: [D.World] -> Integer
runProgSet = loop Nothing
  where
    loop :: Maybe D.World -> [D.World] -> Integer
    loop (Just prev) (next : rest) = if resWaiting || (D.wid result' /= 4)
        then loop (Just result') rest'
        else head $ D.outs result
        -- In the case where there was a pervious run, use it's output
        -- as the input to the current one. Also, push the last one at
        -- the end of the queue
      where
        next'      = next { D.ins = D.ins next ++ D.outs prev } -- Feed next with out of prev
        prev'      = prev { D.outs = [] } -- Clear out the prev's output
        rest'      = rest ++ [prev'] -- Put the prev at the end of the queue
        result     = D.run next'
        resWaiting = D.inWait result
        result'    = result { D.inWait = False }
    loop Nothing (first : rest) = loop (Just result) rest
        -- First time through the loop there is no previous, we'll seed and run the head of the list
        -- with a zero signal, and continue the loop
      where
        first' = first { D.ins = D.ins first ++ [0] }
        result = D.run first' { D.inWait = False }

solve :: [Integer] -> Integer
solve prog = maximum ans
  where
    pss = progSets prog 5 9
    ans = map runProgSet pss
