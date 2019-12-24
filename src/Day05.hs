module Day05 where

    import Data.List (splitAt)
    import AdventData (day05)

{--------
| Types |
--------}

    data Op = Add | Mult | Inp | Outp
            deriving (Show, Enum, Eq)

    -- | From an integer that indicates an opcode, get the opcode and the number of parameters expected
    opOfInt n = case n of 1 -> (Add,2); 2 -> (Mult,2); 3 -> (Inp,1); 4 -> (Outp,1)

    -- | Parameter is either a positional type, where the Int is an address, or
    --   a value type, where the Int is the literal value of the parameter   
    data PosMode = Pos | Immed deriving (Show)
    itoPosMode i = case i of 0 -> Pos; 1 -> Immed

    type Statement = (Op, [(PosMode, Int)])

    type Memory = [Int]

    data World = World { ins    :: [Int]
                       , outs   :: [Int]
                       , mem    :: Memory
                       , offset :: Int
                       } deriving (Show)

{----------\
| Solution |
\----------}

    -- | starting state of the world
    world = World { ins    = [1]
                  , outs   = []
                  , mem    = day05
                  , offset = 0 }

    -- | Read an input from the world
    wRead :: World -> (Int,World)
    wRead w =
        let ins' = ins w in
            case ins' of
                [] -> error "No input left in the world"
                m  -> (head ins', w { ins = tail m })

    -- | Write output to the world
    wWrite :: World -> Int -> World
    wWrite w v = w { outs = outs w ++ [v] }

    toVal :: World -> (PosMode, Int) -> Int
    toVal w (Pos, i) = mem world !! i
    toVal _ (Immed, i) = i

    intToInst :: Int -> World -> Statement
    intToInst i w =
        ((op, nParams), zip modes raws)
        where
            ds = digs i
            l = length ds
            (op,nParams) = opOfInt . undigs $ drop (l - 2) ds
            modes = [itoPosMode (ds !!(l - 3 - p)) | p <- [0..nParams-1]]
            raws = take (length modes) $ drop (offset w) (mem w)

    -- eval gets the answer, still need a function to determine where the answer should go for add and mult and even read
    eval :: World -> Statement -> World
    eval w (Add, params) = toVal w (params !! 0) + toVal w (params !! 1)
        w

    run :: Int -> [Int] -> [Int]
    run pos mem =
        if op /= 99
        then run (pos + 4) (replace pAns (fn (mem!!p1) (mem!!p2)) mem)
        else mem
        where
            (op:p1:p2:pAns:_) = drop pos mem
            fn = case op of 1 -> (+); 2 -> (*); 99 -> \ _ _ -> 0

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
    replace i e xs = case splitAt i xs of
        (before, _:after) -> before ++ e : after
        _ -> xs
