module Day12 where

import AdventData (day12, day12ex01, day12ex02)

type Vec3 = (Int,Int,Int)

data Planet = Planet { pos :: Vec3
                     , vel :: Vec3 }
              deriving (Show, Eq)

newPlanet :: Int -> Int -> Int -> Planet
newPlanet x y z = Planet { pos = (x,y,z), vel = (0,0,0) }

energyOf :: Planet -> Int
energyOf p =
    let (x ,y ,z)  = pos p in
    let (vx,vy,vz) = vel p in
    let pot        = abs x  + abs y  + abs z in
    let kin        = abs vx + abs vy + abs vz in
    pot * kin

deser :: [Vec3] -> [Planet]
deser = map (\p -> Planet { pos = p, vel = (0,0,0) }) 

extractAndGroup :: Eq a => [a] -> [(a, [a])]
extractAndGroup xs = map (\x -> (x, [x' | x' <- xs, x' /= x])) xs

adjustDim :: Int -> Int -> Int -> Int
adjustDim v p po | p - po >  0 = v - 1
adjustDim v p po | p - po <  0 = v + 1
adjustDim v p po               = v

adjustVel :: Planet -> Planet -> Planet
adjustVel p po =
    p { vel =
        (adjustDim vx px pxo
        ,adjustDim vy py pyo
        ,adjustDim vz pz pzo) }
  where
    (vx,vy,vz)      = vel p
    (px,py,pz)      = pos p
    (pxo, pyo, pzo) = pos po

updateP :: Planet -> [Planet] -> Planet
updateP = foldl adjustVel

updateVelocities :: [(Planet, [Planet])] -> [Planet]
updateVelocities = map (uncurry updateP)

applyVelocity :: Planet -> Planet
applyVelocity p =
    let (x,y,z)    = pos p in
    let (vx,vy,vz) = vel p in
    p { pos = (x + vx, y + vy, z + vz)}

applyVelocities :: [Planet] -> [Planet]
applyVelocities = map applyVelocity

step :: [Planet] -> [Planet]
step = applyVelocities . updateVelocities . extractAndGroup

applySteps :: Int -> [Planet] -> [Planet]
applySteps n ps | n > 0 = applySteps (n-1) (step ps)
applySteps 0 ps = ps

solution1 = sum . map energyOf $ applySteps 1000 (deser day12)