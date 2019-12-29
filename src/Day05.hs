module Day05 where

    import Data.List (splitAt)
    import AdventData (day05)
    import Debug.Trace
    
{--------
| Types |
--------}

    data Op = Add | Mult | Inp | Outp | JumpT | JumpF | LessThan | Equals | AdjRBase | Terminate
            deriving (Show, Enum, Eq)

    -- | From an integer that indicates an opcode, get the opcode and the number of parameters expected
    opOfInt :: (Eq a, Num a, Num b, Show a) => a -> (Op, b)
    opOfInt n = case n of
         1 -> (Add,3)
         2 -> (Mult,3)
         3 -> (Inp,1)
         4 -> (Outp,1)
         5 -> (JumpT, 2)
         6 -> (JumpF, 2)
         7 -> (LessThan, 3)
         8 -> (Equals, 3)
         9 -> (AdjRBase, 1)
         99 -> (Terminate,0)
         otherwise -> error ("unknown operand value: " ++ (show n))

    -- | Parameter is either a positional type, where the Int is an address, or
    --   a value type, where the Int is the literal value of the parameter   
    data PosMode = Pos | Immed | Rel deriving (Show)
    itoPosMode i = case i of 0 -> Pos; 1 -> Immed; 2 -> Rel

    type Address = Integer

    -- | Instruction on obtaining a value either in position mode or immediate mode
    type ValueInstruction = (PosMode, Address)

    -- | A statement consists of an operation and zero or more value instructions. the operation dicates
    --   how the ValueInstructions will be parsed
    type Statement = (Op, [ValueInstruction])

    -- | Computer memory modeled by a list of ints. Not great for perf.
    type Memory = [Integer]

    -- | World has inputs and outputs, a memory, and a current offset which is the pointer to the next instruction
    --   to be executed
    data World = World { ins    :: [Integer]
                       , inWait :: Bool     -- used to indicate suspended awaiting input
                       , outs   :: [Integer]
                       , mem    :: Memory
                       , wid    :: Int      -- in 7b, it's useful to know which prog is running
                       , offset :: Integer
                       , rbase  :: Integer
                       } deriving (Show)

{----------\
| Solution |
\----------}

    -- | starting state of the world
    initialWorld
        = World { ins    = [1]
                , inWait = False
                , outs   = []
                , mem    = day05
                , wid    = 0
                , rbase  = 0
                , offset = 0 }

    -- | Similar to the list index operator, except in this application, if you index beyond the 
    --   end of the list, the list is intended to be padded out by zeros. Since this is a read 
    --   operation asking for a location that would be padded, then we send them the default of
    --   zero, even though we haven't padded memory.
    (!!!) :: Memory -> Integer -> Integer
    (!!!) mem addr =
        if addr >= (toInteger $ length mem)
        then 0
        else (mem !! (fromInteger addr))

    -- | Translates a value instruction to an address. Modes are:
    --   immediate: the value instruction is not an address, this is an error in this function
    --   positional: the value represents an address, return the address
    --   relative: the value represents an offset from a relative base. The relative base can be
    --             found in the world context
    toAddr :: World -> ValueInstruction -> Integer
    toAddr _ (Immed, i) = error "Cannot change an immediate mode value instruction to an address"
    toAddr _ (Pos, i) = i
    toAddr world (Rel, i) = (rbase world) + i
    
    -- | Given a world and a value instruction, get the value
    toVal :: World -> ValueInstruction -> Integer
    toVal _ (Immed, i) = i
    toVal world vi = mem world !!! (toAddr world vi)

    -- | Get the statement at the next offset in the world 
    addrToStatement :: World -> Statement
    addrToStatement world =
        (op, zip modes raws)
        where
            raw = mem world !!! (offset world)
            ds =
                let d = digs raw in
                if length d < 5 then (take (5 - length d) $ repeat 0) ++ d else d
            l = length ds
            (op,nParams) = opOfInt . undigs $ drop (l - 2) ds
            modes = [itoPosMode (ds !!(l - 3 - p)) | p <- [0..nParams-1]]
            raws = take (length modes) $ drop (fromInteger $ offset world + 1) (mem world)

    -- | Evaluate the binary operation operation, return a modified world
    binOp :: World -> Op -> [ValueInstruction] -> World
    binOp world op params =
        world {
            mem = writeMem (fromInteger resultAddr) (fn v1 v2) (mem world),
            offset = offset world + 4
        }
      where
        resultAddr = toAddr world $ params !! 2
        fn = case op of Add -> (+); Mult -> (*)
        (v1, v2) = (toVal world (head params), toVal world (params !! 1))

    -- | Read from the World's inputs, write it to the target indicated by the value 
    --   instruction, return modified world
    readOp :: World -> ValueInstruction -> World
    readOp world vi =
        if null inputs
        then world { inWait = True }
        else
            world {
                mem = writeMem (fromInteger addr) (head . ins $ world) (mem world),
                ins = tail $ ins world,
                offset = offset world + 2
            }
      where
        inputs = ins world
        addr = toAddr world vi

    -- | Write the value found by evaluating the value instruction to the World's output, return modified world
    writeOp :: World -> ValueInstruction -> World
    writeOp world param =
        world {
            outs = (outs world) ++ [toVal world param],
            offset = offset world + 2
        }

    jumpOp :: World -> Op -> [ValueInstruction] -> World
    jumpOp world op params =
        world {
            offset = newOffset
        }
      where
        (v1,v2) = (toVal world (head params), toVal world (params !! 1))
        fn = if op == JumpF then (==) else (/=)
        newOffset = if fn v1 0 then v2 else (offset world) + 3

    boolOp :: World -> Op -> [ValueInstruction] -> World
    boolOp world op params =
        world {
            mem  = writeMem addr newVal (mem world),
            offset = offset world + 4
        }
        where
        (v1,v2) = (toVal world (head params), toVal world (params !! 1))
        addr = fromInteger $ toAddr world (params !! 2)
        fn = if op == Equals then (==) else (<)
        newVal = if fn v1 v2 then 1 else 0

    adjRBaseOp :: World -> ValueInstruction -> World
    adjRBaseOp world vi =
        let rb = rbase world + (toVal world vi) in
        world { 
            rbase = rb, 
            offset = offset world + 2 
        }

    -- | select appropriate operation evaluator. The return value is the modified world
    --   and a boolean indicating whether or not the program should continue running. 
    eval :: World -> Statement -> (World, Bool)
    eval world (Add,  params) = (binOp world Add  params, True)
    eval world (Mult, params) = (binOp world Mult params, True)
    eval world (Outp, (param:[])) = (writeOp world param, True)
    eval world (JumpF,params) = (jumpOp world JumpF params, True)
    eval world (JumpT,params) = (jumpOp world JumpT params, True)
    eval world (Equals,params) = (boolOp world Equals params, True)
    eval world (LessThan,params) = (boolOp world LessThan params, True)
    eval world (Terminate, _) = (world, False)
    eval world (AdjRBase, (param:[])) = (adjRBaseOp world param, True)
    eval world (Inp, (param:[])) = (w', not $ inWait w')
      where w' = readOp world param

    run :: World -> World
    run world =
        case eval world (addrToStatement world) of
            (world', True) -> run world'
            (world', False) -> world'

    solution1 = run initialWorld
    solution2 = run initialWorld { ins = [5] }

{--------------------
| Utility Functions |
--------------------}

    -- | Turns an integer into an list of digits
    digs :: Integer -> [Integer]
    digs 0 = []
    digs x = digs (x `div` 10) ++ [x `mod` 10]

    -- | turns a list of digits back into a number
    undigs :: [Integer] -> Integer
    undigs [] = 0
    undigs (n:xs) = n * (10 ^ length xs) + undigs xs

    -- | Replace the Integer at location Int in the list. If the list isn't as long as the index,
    -- pad out the list so that it is
    writeMem :: Int -> Integer -> Memory -> Memory
    writeMem index val list | index < (length list) =
        case splitAt index list of
        (before, _:after) -> before ++ val : after
        _ -> list
    writeMem index val list | index >= (length list) =
        writeMem index val list'
      where
        pad = take (index - (length list) + 1) (repeat 0)
        list' = list ++ pad

