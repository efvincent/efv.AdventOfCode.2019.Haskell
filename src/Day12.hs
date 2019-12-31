module Day12 where

import AdventData (day12)

type Vec3 = (Int,Int,Int)
type Planet = (Int,(Vec3,Vec3))

deser :: [Vec3] -> [Planet]
deser raw = 
    let pv = map (\p -> (p,(0,0,0))) raw in
    zip [0..] pv



