module Day11 where

import AdventData (day11)
import Data.List (groupBy, sortOn)
import qualified Data.Map as M
import qualified IntCode as IC
import Utility

type Pos = (Int, Int)

type Panel = M.Map Pos (Integer, Int)

data Dir
    = UP
    | RIGHT
    | DOWN
    | LEFT
    deriving (Show, Eq, Ord, Enum)

type Color = Integer

data State =
    State
        { world :: IC.Computer
        , position :: Pos
        , direction :: Dir
        , colorNext :: Bool -- True -> expecting color output next
        , panel :: Panel
        }
    deriving (Show)

makeState startColor mem =
    State
        { world = IC.setMemory IC.initialComputer mem
        , position = (0, 0)
        , direction = UP
        , colorNext = True -- first we expect color
        , panel = M.fromList [((0, 0), (startColor, 0))]
        }

-- | Turns to a new direction based on a steering value. Steering is expected to be
-- an integer, but for safety 0 -> left, non-zero -> right
turn :: Integer -> State -> State
turn steer state = state {direction = trn d steer}
  where
    d = direction state
    trn :: Dir -> Integer -> Dir
    trn d 0 | d == UP   = LEFT
    trn d 0             = pred d
    trn d _ | d == LEFT = UP
    trn d _             = succ d

-- | Advance the position according to the current direction
advance :: State -> State
advance state = state {position = adv dir pos}
  where
    pos = position state
    dir = direction state
    adv :: Dir -> Pos -> Pos
    adv UP (x, y)    = (x, y - 1)
    adv RIGHT (x, y) = (x + 1, y)
    adv DOWN (x, y)  = (x, y + 1)
    adv LEFT (x, y)  = (x - 1, y)

-- | Paints the panel the selected color at the current position
paint :: Color -> State -> State
paint color state = state {panel = pnl'}
  where
    pnl = panel state
    loc = position state
    (_, n) = M.findWithDefault (0, 0) loc pnl
    pnl' = M.insert loc (color, n + 1) pnl

-- Gets the current color from state
curColor :: State -> Color
curColor s = ccol
  where
    (ccol, _) = M.findWithDefault (0, 0) (position s) (panel s)

execute :: State -> State
execute startState =
    if runnable
        then execute updatedState
        else updatedState
  where
    (state', inputOp, postRunWorld) =
        case (IC.getOutput (IC.run (world startState)), colorNext startState) of
            ((Just col, w), True) -> (paint col startState, False, w)
            ((Just dir, w), False) -> ((advance . turn dir) startState, False, w)
            ((Nothing, w), _) -> (startState, True, w) -- we're not expecting to consume output
    updatedState =
        if inputOp
            -- program paused for an input operation 
            then state' {world = IC.setInput postRunWorld $ curColor state'}
            -- progam paused for output operation, which we consumed. We
            -- toggle the next expected output, clear the output from world,
            -- and we're ready to continue
            else state' {colorNext = not (colorNext startState), world = postRunWorld}
    runnable = not $ IC.terminated (world updatedState)

drawLine :: Int -> [(Int, Int)] -> String
drawLine maxx = loop str
  where
    str = replicate (maxx + 1) ' '
    loop s ((x, _):rest) = loop (replace x '*' s) rest
    loop s [] = s

drawPanel :: Bool -> Panel -> String
drawPanel pos p = unlines $ map (drawLine maxx) lines
  where
    fn = if pos then (==) else (/=)
    xs = [x | (x, _) <- M.keys p]
    ys = [y | (_, y) <- M.keys p]
    (minx, miny) = (minimum xs, minimum ys)
    maxx = maximum xs - minx
    coords = sortOn snd [(x - minx, y - miny) | ((x, y), (c, _)) <- M.assocs p, fn c 0]
    lines = groupBy (\(_, y) (_, y') -> y == y') coords

init1 = makeState 0 day11

init2 = makeState 1 day11

ans1 = length $ panel $ execute (makeState 0 day11)

ans2 = drawPanel False $ panel $ execute (makeState 1 day11)
