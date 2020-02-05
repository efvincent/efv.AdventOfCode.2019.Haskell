module LearningState where

-- | A scratch file for playing with / understanding State monad which will
-- come in handy for various exercises rather than threading state and recursing
-- which is currently my only option. Not incorrect, but not leveraging 
-- idiomatic Haskell.

import Control.Monad.State
import qualified Data.Map as M

type Memory = [(Int,String)]

varLookUp :: Int -> Memory -> (String, Memory)
varLookUp name mem = case varLookUpList' name mem of
    (Just s) -> (s, mem)
    Nothing -> ("Not found", mem)
  where
    varLookUpList' :: Int -> Memory -> Maybe String
    varLookUpList' name [] = Nothing
    varLookUpList' name ((n,v):xs) = if name == n 
                                        then Just v 
                                        else varLookUpList' name xs

varUpdate :: Int -> String-> Memory -> (String, Memory)
varUpdate name nval mem = 
    let newMem = varUpdate' name nval mem in 
        (nval, newMem)
  where
    varUpdate' :: Int -> String -> Memory -> Memory
    varUpdate' name nval [] = [(name,nval)]
    varUpdate' name nval ((n,v):xs) = if name == n 
                                        then (n,nval):xs
                                        else (n,v):(varUpdate' name nval xs)

run :: Memory -> Memory
run mem = 
    let (v0,m0) = varUpdate 0 "eric" mem in
    let (v1,m1) = varUpdate 1 "cole" m0  in
    let (v2,m2) = varUpdate 2 "karin" m1 in 
    m2

varLookUpS :: Int -> State Memory String
varLookUpS str = do
    mem <- get
    case varLookUpListS' str mem of
      (Just s) -> return s
      Nothing -> return "not found" 
  where
    varLookUpListS' :: Int -> Memory -> Maybe String
    varLookUpListS' name [] = Nothing
    varLookUpListS' name ((n,v):xs) = 
        if name == n 
        then Just v 
        else varLookUpListS' name xs

varUpdateS :: Int -> String-> State Memory String
varUpdateS str val = do
    mem <- get
    put (varUpdate' str val mem)
    return val
  where
    varUpdate' :: Int -> String -> Memory -> Memory
    varUpdate' name nval [] = [(name,nval)]
    varUpdate' name nval ((n,v):xs) = 
        if name == n 
        then (n,nval):xs 
        else (n,v):(varUpdate' name nval xs)

runS :: Memory -> Memory 
runS initMem =
    let (val,mem) = 
                    runState (
                        do 
                            varUpdateS 1 "eric"
                            varUpdateS 2 "karin"
                            varUpdateS 3 "cole"
                            x <- varLookUpS 2
                            varUpdateS 4 x) initMem in 
    mem

-- This means that the state monad functions with two things -
-- functions that take some parameters, and return state and a return value
-- and then the runState function

data Prog = Prog { instructions :: [Int]
                 , accumulator :: Int }
                 deriving (Show, Eq)

continue :: State Prog Bool
continue = do
    prg <- get
    let ins = instructions prg 
    return (ins /= [])

step :: [Int] -> State Prog Bool
step newIns = do
    prg <- get 
    let acc = accumulator prg 
    case newIns ++ instructions prg of 
        (i:rest) -> do 
            let prg' = prg { accumulator = acc + i, instructions = rest }
            put prg'
            step []
        [] -> return False

p = Prog { accumulator = 0, instructions = [1,5,3,5,2,4] }

rp prog = 
    runState (step []) prog

type Ingredients = M.Map String Int

addIng :: String -> Int -> State Ingredients ()
addIng key amt = do
    ings <- get
    let amt' = case M.lookup key ings of
            Just v -> v + amt 
            Nothing -> amt 
    put $ M.insert key amt' ings
    return ()

rawIngs :: [(String, Int)]
rawIngs = [("eggs", 2), ("milk", 1), ("eggs", 3), ("sugar", 4), ("vanilla", 7)]

workIngs =
    let (_,newIngs) = 
            runState (do
                mapM_ (uncurry addIng) rawIngs
                ) M.empty
    in newIngs