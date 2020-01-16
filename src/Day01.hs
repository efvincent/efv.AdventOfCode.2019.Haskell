module Day01 where

import           AdventData (day01)

amounts = map (\s -> read s :: Integer) (lines day01)

fuelReq :: Integer -> Integer
fuelReq n = div n 3 - 2

part1 = sum $ map fuelReq amounts

deepFuelReq :: Integer -> Integer
deepFuelReq n | n <= 0 = 0
deepFuelReq n          = (if req < 0 then 0 else req) + deepFuelReq req where req = fuelReq n

part2 = sum (map deepFuelReq amounts)
