module Day13 where

import           IntCode
import qualified Data.Map as M
import           AdventData (day13)
import           Data.List (sort)
import           Utility

data Tile = Empty | Wall | Block | HPaddle | Ball deriving (Show, Eq, Ord)
data OutputType = X | Y | TileID deriving (Show, Enum)
type Pos = (Integer,Integer)

intToTile :: Integer -> Tile
intToTile 0 = Empty
intToTile 1 = Wall
intToTile 2 = Block
intToTile 3 = HPaddle
intToTile 4 = Ball

data GameState = GameState { tiles  :: M.Map Pos Tile
                           , score  :: Integer
                           , ballX  :: Integer
                           , paddleX:: Integer
                           , comp   :: Computer
                           , buffer :: [Integer] }
                           deriving Show

initGame = GameState { tiles   = M.empty
                     , score   = 0
                     , comp    = initialComputer { mem = day13 }
                     , ballX   = 0
                     , paddleX = 0
                     , buffer  = [] }

runGame :: GameState -> GameState
runGame gs =
    let c = run (comp gs) in
    if terminated c then gs { comp = c }
    else if outWait c then
        case getOutput c of
            (Just val, c') -> runGame $ handleOutput gs val c'
            (Nothing, _)   -> error "expecting output, but there's none"
    else if inWait c then
        let bx = ballX gs in
        let px = paddleX gs in
        let inp = case bx - px of
             d | d > 0 ->  1
             d | d < 0 -> -1
             _         ->  0 in
        runGame $ gs { comp = setInput inp (comp gs) }
    else
        error "no output, and not terminated... wtf?"

handleOutput :: GameState -> Integer -> Computer -> GameState
handleOutput gs val c =
    let b = val : buffer gs in
    case length b of
        3 ->
            let [other,y,x] = b in
            if x == -1 then 
                gs { comp = c, buffer = [], score = other }
            else            
                let tid = intToTile other in
                let gs' = gs { comp = c, buffer = [], tiles = M.insert (x,y) tid (tiles gs) } in
                case tid of
                    HPaddle -> gs' { paddleX = x }
                    Ball ->    gs' { ballX = x }
                    _ ->       gs'
        _ -> gs { comp = c , buffer = b }

solve_d13_1 =
    let gs = runGame initGame in
    let t  = sort $ map snd $ M.assocs $ tiles gs in
    length $ filter (== Block) t

solve_d13_2 =
    score $ runGame gs'
  where
    gs  = initGame
    c   = comp gs
    gs' = gs { comp = c { mem = replace 0 2 (mem c) } }
