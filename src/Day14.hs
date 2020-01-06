module Day14 where

import           AdventData        (day14, day14ex01, day14ex02, day14ex03,
                                    day14ex04, day14ex05)
import           Control.Monad.Zip (mzip)
import           Data.List.Split   (splitOn)
import qualified Data.Map          as M
import           Utility           (stoiM)

type Quantity   = Int
type Symbol     = String
type Ingredient = (Quantity, Symbol)
type Recipe     = (Ingredient, [Ingredient])
type Cookbook   = [Recipe]
type Cookbook1  = M.Map Symbol Recipe

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

