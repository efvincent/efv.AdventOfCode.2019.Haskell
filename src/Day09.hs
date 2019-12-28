module Day09 where

import qualified Day05 as D
import AdventData (day09ex01,day09ex02,day09ex03,day09)

wex01 = D.initialWorld { D.ins = [], D.mem = day09ex01 }
wex02 = wex01 { D.mem = day09ex02 }
wex03 = wex01 { D.mem = day09ex03 }

solution1 = 
    let w = D.initialWorld  { D.ins = [1], D.mem = day09 } in
    D.run w

solution2 =
    let w = D.initialWorld  { D.ins = [2], D.mem = day09 } in
    D.run w