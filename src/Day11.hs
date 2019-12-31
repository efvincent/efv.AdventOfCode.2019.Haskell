module Day11 where

import qualified Data.Map as M
import qualified Day05 as D
import           AdventData (day11)
import           Debug.Trace

type Pos = (Int,Int)
type Panel = M.Map Pos (Integer,Int)
data Dir = N | E | S | W deriving (Show, Eq, Ord, Enum)
type Color = Integer

data State = State { world       :: D.World
                   , position    :: Pos
                   , direction   :: Dir
                   , panel       :: Panel }
                   deriving (Show)

initWorld =
    D.World { D.ins    = []
            , D.inWait = False
            , D.outs   = []
            , D.mem    = day11
            , D.wid    = 0
            , D.rbase  = 0
            , D.offset = 0 }

initState =
    State { world = initWorld
          , position = (0,0)
          , direction = N
          , panel = M.empty }

-- | Turns to a new direction based on a steering value. Steering is expected to be
-- an integer, but for safety 0 -> left, non-zero -> right
turn :: Integer -> State -> State
turn steer state =
    state { direction = trn d steer}
  where
    d = direction state
    trn :: Dir -> Integer -> Dir
    trn d 0 | d == N = W
    trn d 0 = pred d
    trn d _ | d == W = N
    trn d _ = succ d

-- | Advance the position according to the current direction
advance :: State -> State
advance state =
    state { position = adv dir pos }
  where
    pos = position state
    dir = direction state
    adv :: Dir -> Pos -> Pos
    adv N (x,y) = (x,y-1)
    adv E (x,y) = (x+1,y)
    adv S (x,y) = (x,y+1)
    adv W (x,y) = (x-1,y)

-- | Paints the panel the selected color at the current position
paint :: Color -> State -> State
paint color state =
    state { panel = pnl' }
  where
    pnl    = panel state
    loc    = position state
    (_,n)  = M.findWithDefault (0,0) loc pnl
    pnl'   = M.insert loc (color,n + 1) pnl

curColor :: State -> Color
curColor s =
    ccol
  where
    (ccol,_) = M.findWithDefault (0,0) (position s) (panel s)

runWorld :: D.World -> Color -> D.World
runWorld w c =
    if cont'
        then
            if length output >= 2 then w'
            else runWorld w' c
        else w'
  where
    w' =
        -- trace ("ins:" ++ show [c])
        D.run $ w { D.ins = [c] }
    output = D.outs w'
    cont = D.inWait w'
    cont' =
        trace ("outs:" ++ show output ++ "  cont:" ++ show cont)
        cont

execute :: State -> Int -> Int -> State
execute state c maxc =
    if continue && c < maxc
    then execute state'' (c+1) maxc
    else state''
  where
    w = runWorld (world state) (curColor state)
    (color:steer:_) = D.outs w
    state' = (advance . turn steer . paint color) state
    w' = w { D.outs = [], D.ins = [curColor state'] }
    state'' = state' { world = w' }
    continue =
{-       trace ("dir=" ++ show (direction state'')
          ++ " pos=" ++ show (position state'')
          ++ " ins=" ++ show (D.ins w')
          ++ " panel=" ++ show (panel state'')
          ) -}
      D.inWait w'
