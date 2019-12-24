module Day05 where

    import Data.List (splitAt)
    import AdventData (day05)

{--------
| Types |
--------}

    data Op = Add | Mult | Inp | Outp | JumpT | JumpF | LessThan | Equals | Terminate
            deriving (Show, Enum, Eq)

    -- | From an integer that indicates an opcode, get the opcode and the number of parameters expected
    opOfInt n = case n of
         1 -> (Add,3)
         2 -> (Mult,3)
         3 -> (Inp,1)
         4 -> (Outp,1)
         5 -> (JumpT, 2)
         6 -> (JumpF, 2)
         7 -> (LessThan, 3)
         8 -> (Equals, 3)
         99 -> (Terminate,0)
         otherwise -> error ("unknown operand value: " ++ (show n))

    -- | Parameter is either a positional type, where the Int is an address, or
    --   a value type, where the Int is the literal value of the parameter   
    data PosMode = Pos | Immed deriving (Show)
    itoPosMode i = case i of 0 -> Pos; 1 -> Immed

    type Address = Int

    -- | Instruction on obtaining a value either in position mode or immediate mode
    type ValueInstruction = (PosMode, Address)

    -- | A statement consists of an operation and zero or more value instructions. the operation dicates
    --   how the ValueInstructions will be parsed
    type Statement = (Op, [ValueInstruction])

    -- | Computer memory modeled by a list of ints. Not great for perf.
    type Memory = [Int]

    -- | World has inputs and outputs, a memory, and a current offset which is the pointer to the next instruction
    --   to be executed
    data World = World { ins    :: [Int]
                       , outs   :: [Int]
                       , mem    :: Memory
                       , offset :: Int
                       } deriving (Show)

{----------\
| Solution |
\----------}

    -- | starting state of the world
    initialWorld 
        = World { ins    = [1]
                , outs   = []
                , mem    = day05
                , offset = 0 }

    -- | Given a world and a value instruction, get the value
    toVal :: World -> ValueInstruction -> Int
    toVal world (Pos, i) = mem world !! i
    toVal _ (Immed, i) = i

    -- | Get the statement at the next offset in the world 
    addrToStatement :: World -> Statement
    addrToStatement world =
        (op, zip modes raws)
        where
            raw = mem world !! (offset world)
            ds = 
                let d = digs raw in
                if length d < 5 then (take (5 - length d) $ repeat 0) ++ d else d 
            l = length ds
            (op,nParams) = opOfInt . undigs $ drop (l - 2) ds                   
            modes = [itoPosMode (ds !!(l - 3 - p)) | p <- [0..nParams-1]]
            raws = take (length modes) $ drop (offset world + 1) (mem world)

    -- | Evaluate the binary operation operation, return a modified world
    binOp :: World -> Op -> [ValueInstruction] -> World
    binOp world op params =
        world {
            mem = replace resultAddr (fn v1 v2) (mem world),
            offset = offset world + 4
        }
      where 
        resultAddr = snd $ params !! 2
        fn = case op of Add -> (+); Mult -> (*)
        (v1, v2) = (toVal world (head params), toVal world (params !! 1))

    -- | Read from the World's inputs, write it to the target address, return modified world
    readOp :: World -> Address -> World
    readOp world addr =
        world {
            mem = replace addr (head . ins $ world) (mem world),
            ins = tail $ ins world,
            offset = offset world + 2
        }

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
            mem  = replace addr newVal (mem world),
            offset = offset world + 4            
        }
        where
        (v1,v2) = (toVal world (head params), toVal world (params !! 1))
        addr = snd (params !! 2)
        fn = if op == Equals then (==) else (<)
        newVal = if fn v1 v2 then 1 else 0

    -- | select appropriate operation evaluator
    eval :: World -> Statement -> (World, Bool)
    eval world (Add,  params) = (binOp world Add  params, True)
    eval world (Mult, params) = (binOp world Mult params, True)
    eval world (Inp,  ((Pos,addr):[])) = (readOp world addr, True)
    eval world (Outp, (param:[])) = (writeOp world param, True)        
    eval world (JumpF,params) = (jumpOp world JumpF params, True)
    eval world (JumpT,params) = (jumpOp world JumpT params, True)
    eval world (Equals,params) = (boolOp world Equals params, True)
    eval world (LessThan,params) = (boolOp world LessThan params, True)    
    eval world (Terminate, _) = (world, False)

    run :: World -> World
    run world =
        case eval world (addrToStatement world) of
            (world', True) -> run world'
            (world', False) -> world

    solution1 = run initialWorld
    solution2 = run initialWorld { ins = [5] }

{--------------------
| Utility Functions |
--------------------}

    -- | Turns an integer into an list of digits
    digs :: Int -> [Int]
    digs 0 = []
    digs x = digs (x `div` 10) ++ [x `mod` 10]

    -- | turns a list of digits back into a number
    undigs :: [Int] -> Int
    undigs [] = 0
    undigs (n:xs) = n * (10 ^ length xs) + undigs xs

    replace :: Int -> a -> [a] -> [a]
    replace index val list = case splitAt index list of
        (before, _:after) -> before ++ val : after
        _ -> list
