module Day14 where

import           AdventData        (day14, day14ex01, day14ex02, day14ex03,
                                    day14ex04, day14ex05)
import           Control.Monad.Zip (mzip)
import           Data.List.Split   (splitOn)
import           Data.List         (find)
import qualified Data.Map          as M
import           Utility           (stoiM)

type Quantity   = Int
type Symbol     = String
type Ingredient = (Quantity, Symbol)
type Recipe     = (Ingredient, [Ingredient])
type Cookbook   = [Recipe]

deser :: String -> Maybe Cookbook
deser raw =
    mapM deserLine rawRecipes
  where
    rawRecipes = map (splitOn "=>") $ lines raw :: [[String]]

deserLine :: [String] -> Maybe Recipe
deserLine [s1,s2] =
    mzip output ingredients
  where
    ingredients = mapM deserPair (filter (/= "") $ splitOn "," s1)
    output = deserPair s2
deserLine _ = Nothing

deserPair :: String -> Maybe Ingredient
deserPair s =
    case length parts of
        2 -> let [p1,p2] = parts in
             let quantM = stoiM p1 in
             case quantM of
                 Just quant -> Just (quant, p2)
                 Nothing    -> Nothing
        _ -> Nothing
  where
    parts = filter (/= "") $ splitOn " " s

getRecipe :: Symbol -> Maybe Cookbook -> Maybe Recipe
getRecipe sym cookbookM = 
    do 
        cb <- cookbookM
        find (\((_,key),_) -> key == sym) cb

batchCount :: Int -> Int -> Int
batchCount batchSize amtNeeded =
    let fullBatches = amtNeeded `div` batchSize in
    if amtNeeded `mod` batchSize == 0 then fullBatches else fullBatches + 1

-- this is not quite right - need to get the total amount for each ingredient, and then run it through
-- the batches formula. IE rather than getting 4 batches of 10 of "A" because 4 different recipes need
-- 7 "A"s, we add up all the requirements to find 28 total "A"s would be needed, and for that we can
-- produce 3 batches and only have 2 wasted as opposed to having 40 "A" and 12 wasted.
getOreCost :: [Ingredient] -> Maybe Cookbook -> Maybe Int
getOreCost [(amt,"ORE")] _ = Just amt
getOreCost ((amt,ing):ings) cb =
    do
        ((batchSize, curSym), curIngs) <- getRecipe ing cb
        let batches = batchCount batchSize amt
        curCost <- getOreCost curIngs cb
        rest <- getOreCost ings cb
        return $ batches * curCost + rest
getOreCost [] _ = Just 0

d = deser day14ex01