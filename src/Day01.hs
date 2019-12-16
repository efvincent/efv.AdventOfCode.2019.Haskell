module Day01 where

import AdventData (day01)

amounts = map (\s -> read s::Int) (lines day01)

fuelReq :: Int -> Int
fuelReq n = div n 3 - 2

part1 = sum $ map fuelReq amounts

deepFuelReq :: Int -> Int
deepFuelReq n | n <= 0 = 0
deepFuelReq n = (if req < 0 then 0 else req) + deepFuelReq req
    where req = fuelReq n

part2 = sum (map deepFuelReq amounts)

