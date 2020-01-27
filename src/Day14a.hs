module Day14a where

import           AdventData          (day14, day14ex01, day14ex02, day14ex03,
                                      day14ex04, day14ex05)
import           Control.Monad.State
import           Data.List.Split     (splitOn)
import qualified Data.Map            as M
import           Utility             (stoiM)

type Quantity = Int
type Symbol = String
data Recipe = Recipe { symbol      :: Symbol
                     , yields      :: Int
                     , ingredients :: [(Symbol, Quantity) ] }
                     deriving (Show, Eq)
type Cookbook = M.Map Symbol Recipe

deserCookbook :: String -> Cookbook
deserCookbook s =
    M.fromList . map (\r -> (symbol r, r)) . map deserRecipe . lines $ s

deserRecipe :: String -> Recipe
deserRecipe s =
    Recipe { symbol = sym, yields = q, ingredients = ings }
  where
    [rIngs,recipeFor] = splitOn "=>" s
    (sym,q) = deserSymQuant recipeFor
    ings = map deserSymQuant . splitOn "," $ rIngs

deserSymQuant :: String -> (Symbol, Quantity)
deserSymQuant s = let [q,sym] = words s in (sym, read q)

mkShoppingList :: Symbol -> Quantity -> Cookbook -> [(Symbol, Quantity)]
mkShoppingList sym quant cb =
    ings 
  where
    ings = 
        case M.lookup sym cb of 
            Just r -> 
                let ings = ingredients r in
                concatMap (\(s,q) -> [(s, (quant * q))] ++ mkShoppingList s (quant * q) cb) ings
            Nothing   -> []