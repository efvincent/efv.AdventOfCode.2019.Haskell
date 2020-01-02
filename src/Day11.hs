module Day11 where

import qualified Data.Map as M
import qualified Day05 as D
import           AdventData (day11)

type Pos = (Int,Int)
type Panel = M.Map Pos (Integer,Int)
data Dir = UP | RIGHT | DOWN | LEFT deriving (Show, Eq, Ord, Enum)
type Color = Integer

data State = State { world       :: D.World
                   , position    :: Pos
                   , direction   :: Dir
                   , colorNext   :: Bool        -- True -> expecting color output next
                   , panel       :: Panel }
                   deriving (Show)

initState =
    State { world = D.initialWorld { D.mem = day11 }
          , position = (0,0)
          , direction = UP
          , colorNext = True        -- first we expect color
          , panel = M.empty }

-- | Turns to a new direction based on a steering value. Steering is expected to be
-- an integer, but for safety 0 -> left, non-zero -> right
turn :: Integer -> State -> State
turn steer state =
    state { direction = trn d steer}
  where
    d = direction state
    trn :: Dir -> Integer -> Dir
    trn d 0 | d == UP = LEFT
    trn d 0 = pred d
    trn d _ | d == LEFT = UP
    trn d _ = succ d

-- | Advance the position according to the current direction
advance :: State -> State
advance state =
    state { position = adv dir pos }
  where
    pos = position state
    dir = direction state
    adv :: Dir -> Pos -> Pos
    adv UP (x,y) = (x,y-1)
    adv RIGHT (x,y) = (x+1,y)
    adv DOWN (x,y) = (x,y+1)
    adv LEFT (x,y) = (x-1,y)

-- | Paints the panel the selected color at the current position
paint :: Color -> State -> State
paint color state =
    state { panel = pnl' }
  where
    pnl    = panel state
    loc    = position state
    (_,n)  = M.findWithDefault (0,0) loc pnl
    pnl'   = M.insert loc (color,n + 1) pnl

-- Gets the current color from state
curColor :: State -> Color
curColor s = ccol
  where (ccol,_) = M.findWithDefault (0,0) (position s) (panel s)

execute :: State -> State
execute startState =
    if runnable 
    then execute updatedState 
    else updatedState
  where
    postRunWorld = D.run (world startState)
    (state',inputOp) = 
        case (D.output postRunWorld,colorNext startState) of
            (Just col, True) -> 
                (paint col startState, False)
            (Just dir, False) ->
                ((advance . turn dir) startState, False)
            (Nothing,_) -> (startState, True) -- we're not expecting to consume output

    updatedState =
        if inputOp then
            -- program paused for an input operation 
            state' {world = postRunWorld { D.input = Just $ curColor state'
                                          , D.inWait = False
                                          , D.output = Nothing }}
        else
            -- progam paused for output operation, which we consumed. We
            -- toggle the next expected output, clear the output from world,
            -- and we're ready to continue
            state' { colorNext = not (colorNext startState) 
                   , world = postRunWorld { D.output = Nothing 
                                          , D.inWait = False } }
                   
    runnable = not $ D.terminated (world updatedState)
