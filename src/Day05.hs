module Day05 where

import AdventData (day05)
import IntCode

world = initialWorld { mem = day05 }

stepThru :: World -> IO ()
stepThru world = do
    let w = run world
    case (terminated w, outWait w) of
        (False, True) ->
            do 
                let (out, w') = getOutput w
                print out
                stepThru w
        (True, _) -> print "Complete"

main::IO() 
main = do
    stepThru $ world { input = Just 1 }
    stepThru $ world { input = Just 5 }
