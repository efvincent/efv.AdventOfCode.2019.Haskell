module Day14a where

import           AdventData          (day14, day14ex01, day14ex02, day14ex03,
                                      day14ex04, day14ex05)
import           Control.Monad.State
import           Data.List.Split     (splitOn, splitWhen)
import           Data.List.Extra     (sortOn, group, groupOn, intercalate)
import qualified Data.Map            as M
import           Utility             (stoiM)

type Quantity = Int
type Symbol = String
type SymQ = (Symbol, Quantity)

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

top :: Cookbook -> [SymQ] -> [SymQ] -> ([SymQ],[SymQ])
top _ [] reduced = ([],reduced)
top cb (symq@(sym,quant):xs) reduced =
    if isMadeOfOre cb sym then top cb xs (symq:reduced)
    else top cb (xs ++ ings) reduced
  where
    Just recip = M.lookup sym cb
    ings = (\(s,q) -> (s,q*quant)) <$> ingredients recip :: [SymQ]
    
addsq :: (Symbol,Quantity) -> (Symbol,Quantity) -> (Symbol,Quantity)
addsq (_,q1) (s,q2) = (s,q1 + q2)

batches :: Integral a => a -> a -> a
batches q y = q `div` y + if q `mod` y == 0 then 0 else 1

oreQuant :: Cookbook -> SymQ -> Quantity
oreQuant cookbook (sym,q) =
    b * oreQ
  where
    Just r = M.lookup sym cookbook
    y = yields r
    b = batches q y
    Just oreQ = lookup "ORE" (ingredients r) 

{-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                               !
!   ITS THE FRACKING LEFTOVERS  !
!                               !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
we have to re-use the leftovers wow.
-}
solve :: String -> Int
solve raw =
    sum . map (oreQuant cookbook) $ simpleIngredients
  where
    cookbook = deserCookbook raw 
    simpleIngredients = simplify cookbook

simplify :: Cookbook -> [SymQ]
simplify cb = map (foldl addsq ("",0)) . groupOn fst . sortOn fst . snd $ top cb [("FUEL",1)] []

step3 :: String -> SymQ -> [[SymQ]]
step3 raw symq =
    let cookbook = deserCookbook raw in
    groupOn fst . sortOn fst. snd $ top cookbook [symq] []

pIng :: SymQ -> String
pIng (s,q) = show q ++ s

pIngs :: [SymQ] -> String
pIngs ings = intercalate ", " . map pIng $ ings

pRecipe :: Recipe -> String
pRecipe r =
    ings ++ " => " ++ (show $ yields r) ++ " " ++ (symbol r)
  where
    ings = intercalate ", " . map (\(s,q) -> show q ++ " " ++ s) $ ingredients r

pCookbook :: Cookbook -> String
pCookbook cb =
    let l = map snd . M.toList $ cb in
    intercalate "\n" . map pRecipe $ l

printCb :: Cookbook -> IO ()
printCb = putStrLn . pCookbook

printCbS :: String -> IO ()
printCbS = putStrLn . pCookbook . deserCookbook
