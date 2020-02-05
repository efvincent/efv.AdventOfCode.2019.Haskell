module Day14a where

import           AdventData          (day14, day14ex01, day14ex02, day14ex03,
                                      day14ex04, day14ex05)
import           Control.Monad.State
import           Data.List.Split     (splitOn, splitWhen)
import qualified Data.Map            as M
import           Utility             (stoiM)

type Quantity = Int
type Symbol = String
data Recipe = Recipe { symbol      :: Symbol
                     , yields      :: Int
                     , fundemental :: Bool
                     , ingredients :: [(Symbol, Quantity) ] }
                     deriving (Show, Eq)
type Cookbook = M.Map Symbol Recipe

deserCookbook :: String -> Cookbook
deserCookbook s =
    M.fromList . map (\r -> (symbol r, r)) . map deserRecipe . lines $ s

deserRecipe :: String -> Recipe
deserRecipe s =
    Recipe { symbol = sym, yields = q, fundemental = fund, ingredients = ings }
  where
    [rIngs,recipeFor] = splitOn "=>" s
    (sym,q) = deserSymQuant recipeFor
    ings = map deserSymQuant . splitOn "," $ rIngs
    fund = any (\(s,_) -> s == "ORE") ings

deserSymQuant :: String -> (Symbol, Quantity)
deserSymQuant s = let [q,sym] = words s in (sym, read q)

isMadeOfOre :: M.Map String Recipe -> String -> Bool
isMadeOfOre cb sym =
    case fund of 
        Just f -> f
        Nothing -> False
  where
    fund = do
        r <- M.lookup sym cb
        return $ fundemental r

solve :: Cookbook -> [(Symbol, Quantity)]
solve cb =
    undefined
  where
    madeOfOre = filter (isMadeOfOre cb) . map fst . M.toList $ cb

msl :: Cookbook -> [String] -> [(Symbol,Quantity)] -> [(Symbol,Quantity)]
msl _ _ [] = []
msl cb madeOfOre ((sym,n):xs) = 
    if sym == "ORE" then [] else
    msl cb $ xs ++ subIngs
  where 
    Just r = M.lookup sym cb
    subIngs =
        case fundemental r of
            True -> [(sym,n)]
            _    -> ingredients r
    

cb = deserCookbook day14ex01      
lu :: Symbol -> Maybe Recipe
lu = flip M.lookup cb

madeOfOre = filter (isMadeOfOre cb) . map fst . M.toList $ cb