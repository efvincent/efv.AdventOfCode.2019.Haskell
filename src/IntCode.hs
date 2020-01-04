module IntCode ( run
               , initialComputer
               , Computer(inWait,outWait,terminated)
               , stepThru
               , getOutput
               , setInput
               , setMemory
               , setMemoryAt) where

    import Data.List (splitAt)
    import Utility 

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
         _ -> error ("unknown operand value: " ++ show n)

    -- | Parameter is either a positional type, where the Int is an address, or
    -- a value type, where the Int is the literal value of the parameter   
    data PosMode = Pos | Immed | Rel deriving (Show)
    itoPosMode i = case i of 0 -> Pos; 1 -> Immed; 2 -> Rel

    type Address = Integer

    -- | Instruction on obtaining a value either in position mode or immediate mode
    type ValueInstruction = (PosMode, Address)

    -- | A statement consists of an operation and zero or more value instructions. 
    -- the operation dicates how the ValueInstructions will be parsed
    type Statement = (Op, [ValueInstruction])

    -- | Computer memory modeled by a list of ints. Not great for perf.
    type Memory = [Integer]

    -- | Computer has inputs and outputs, a memory, and a current offset which is the pointer 
    -- to the next instruction to be executed
    data Computer = Computer { input      :: Maybe Integer
                             , inWait     :: Bool
                             , output     :: Maybe Integer
                             , outWait    :: Bool
                             , terminated :: Bool
                             , mem        :: Memory
                             , wid        :: Int      -- in 7b   , it's useful to know which prog is running
                             , offset     :: Integer
                             , rbase      :: Integer
                             } deriving (Show, Eq)

{----------\
| Solution |
\----------}

    -- | starting state of the computer
    initialComputer
        = Computer { input      = Nothing
                   , inWait     = False
                   , outWait    = False
                   , terminated = False
                   , output     = Nothing
                   , mem        = []
                   , wid        = 0
                   , rbase      = 0
                   , offset     = 0 }

    -- | Similar to the list index operator, except in this application, if you index beyond the 
    --   end of the list, the list is intended to be padded out by zeros. Since this is a read 
    --   operation asking for a location that would be padded, then we send them the default of
    --   zero, even though we haven't padded memory.
    (!!!) :: Memory -> Integer -> Integer
    (!!!) mem addr =
        if addr >= toInteger (length mem)
        then 0
        else mem !! fromInteger addr

    -- | Translates a value instruction to an address. Modes are:
    --   immediate: the value instruction is not an address, this is an error in this function
    --   positional: the value represents an address, return the address
    --   relative: the value represents an offset from a relative base. The relative base can be
    --             found in the computer context
    toAddr :: Computer -> ValueInstruction -> Integer
    toAddr _ (Immed, i) = error "Cannot change an immediate mode value instruction to an address"
    toAddr _ (Pos, i) = i
    toAddr comp (Rel, i) = rbase comp + i

    -- | Given a computer and a value instruction, get the value
    toVal :: Computer -> ValueInstruction -> Integer
    toVal _ (Immed, i) = i
    toVal comp vi = mem comp !!! toAddr comp vi

    -- | Get the statement at the next offset in the computer 
    addrToStatement :: Computer -> Statement
    addrToStatement comp =
        (op, zip modes raws)
        where
            raw = mem comp !!! offset comp
            ds =
                let d = digs raw in
                if length d < 5 then replicate (5 - length d) 0 ++ d else d
            l = length ds
            (op,nParams) = opOfInt . undigs $ drop (l - 2) ds
            modes = [itoPosMode (ds !!(l - 3 - p)) | p <- [0..nParams-1]]
            raws = take (length modes) $ drop (fromInteger $ offset comp + 1) (mem comp)

    -- | Evaluate the binary operation operation, return a modified computer
    binOp :: Computer -> Op -> [ValueInstruction] -> Computer
    binOp comp op params =
        comp {
            mem = writeMem (fromInteger resultAddr) (fn v1 v2) (mem comp),
            offset = offset comp + 4
        }
      where
        resultAddr = toAddr comp $ params !! 2
        fn = case op of Add -> (+); Mult -> (*)
        (v1, v2) = (toVal comp (head params), toVal comp (params !! 1))

    -- | Read from the Computer's inputs, write it to the target indicated by the value 
    --   instruction, return modified computer
    readOp :: Computer -> ValueInstruction -> Computer
    readOp comp vi =
        case input comp of
            Nothing -> comp { inWait = True }
            Just inVal ->
                let addr = toAddr comp vi in
                comp { mem = writeMem (fromInteger addr) inVal (mem comp)
                      , input = Nothing
                      , inWait = False
                      , offset = offset comp + 2 }

    -- | Write the value found by evaluating the value instruction to the Computer's output, 
    -- return modified computer. Note the latest output is at the end of the output list, 
    -- and the first thing returned is at the head. Readers of output scan just pop the
    -- head off the list to get the output in the order generated.
    writeOp :: Computer -> ValueInstruction -> Computer
    writeOp comp param =
        comp {
            output = Just $ toVal comp param,
            outWait = True,
            offset = offset comp + 2
        }

    jumpOp :: Computer -> Op -> [ValueInstruction] -> Computer
    jumpOp comp op params =
        comp {
            offset = newOffset
        }
      where
        (v1,v2)   = (toVal comp (head params), toVal comp (params !! 1))
        fn        = if op == JumpF then (==) else (/=)
        newOffset = if fn v1 0 then v2 else offset comp + 3

    boolOp :: Computer -> Op -> [ValueInstruction] -> Computer
    boolOp comp op params =
        comp {
            mem    = writeMem addr newVal (mem comp),
            offset = offset comp + 4
        }
        where
        (v1,v2) = (toVal comp (head params), toVal comp (params !! 1))
        addr    = fromInteger $ toAddr comp (params !! 2)
        fn      = if op == Equals then (==) else (<)
        newVal  = if fn v1 v2 then 1 else 0

    adjRBaseOp :: Computer -> ValueInstruction -> Computer
    adjRBaseOp comp vi =
        let rb = rbase comp + toVal comp vi in
        comp {
            rbase = rb,
            offset = offset comp + 2
        }

    -- | select appropriate operation evaluator. The return value is the modified computer
    --   and a boolean indicating whether or not the program has halted. 
    eval :: Computer -> Statement -> Computer
    eval comp (Inp, [param])         = readOp comp param
    eval comp (Outp, [param])        = writeOp comp param
    eval comp (Add,  params)         = binOp comp Add  params
    eval comp (Mult, params)         = binOp comp Mult params
    eval comp (JumpF,params)         = jumpOp comp JumpF params
    eval comp (JumpT,params)         = jumpOp comp JumpT params
    eval comp (Equals,params)        = boolOp comp Equals params
    eval comp (LessThan,params)      = boolOp comp LessThan params
    eval comp (AdjRBase, [param])    = adjRBaseOp comp param
    eval comp (Terminate, _)         = comp { terminated = True }

    run :: Computer -> Computer
    run comp =
        case (evaluatedComputer, stop) of
            (comp', False) -> run comp'
            (comp', True)  -> comp'
      where
        evaluatedComputer = eval comp (addrToStatement comp)
        halt = terminated evaluatedComputer
        stop = inWait evaluatedComputer || outWait evaluatedComputer || terminated evaluatedComputer

{--------------------
| Utility Functions |
--------------------}

    -- | Sets the input in the computer state, clears the
    -- flag indicating the computer is waiting for input
    setInput :: Computer -> Integer -> Computer
    setInput comp i =
        comp { input = Just i
              , inWait = False }

    -- | Retrieves putput from the computer state, clears the
    -- flag indicating the computer is awaiting output to be consumed
    getOutput :: Computer -> (Maybe Integer, Computer)
    getOutput comp =
        (output comp, comp { output = Nothing
                             , outWait = False })

    setMemory :: Computer -> Memory -> Computer
    setMemory comp m = comp { mem = m}

    setMemoryAt :: Computer -> Int -> Integer -> Computer
    setMemoryAt comp location value =
        let m = mem comp in
        let m' = replace location value m in
        setMemory comp m'

    stepThru :: Computer -> IO ()
    stepThru comp = do
        let w = run comp
        case (terminated w, outWait w) of
            (False, True) ->
                do 
                    let (out, w') = getOutput w
                    print out
                    stepThru w
            (True, _) -> print "Complete"

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
    writeMem index val list | index < length list =
        case splitAt index list of
        (before, _:after) -> before ++ val : after
        _ -> list
    writeMem index val list | index >= length list =
        writeMem index val list'
      where
        pad = replicate (index - length list + 1) 0
        list' = list ++ pad
