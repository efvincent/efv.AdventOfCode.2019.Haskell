module Day05 where

import           AdventData (day05)
import           IntCode

computer = setMemory initialComputer day05

stepThru' :: Computer -> IO ()
stepThru' comp = do
    let w = run comp
    case (terminated w, outWait w) of
        (False, True) -> do
            let (out, w') = getOutput w
            print out
            stepThru w
        (True, _) -> print "Complete"

solveD5 :: IO ()
solveD5 = do
    stepThru $ setInput computer 1
    stepThru $ setInput computer 5
