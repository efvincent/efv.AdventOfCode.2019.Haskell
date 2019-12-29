# Advent of Code 2019
Use Advent as exercises while learning Haskell.

## Notes / Experiences
Since this is a Haskell Learning experience, I'm going to do a little bit of a diary on how each exercise went. Starting at day 1, I've had almost no experience with Haskell. I've installed it, gone through a few pages of getting started from here and there. Timing is right with my being reminded of Advent of code and my starting to learn Haskell, so here we are.

### [Day 01](https://adventofcode.com/2019/day/1)
Getting the project set up - at first I was just using "loose" `*.hs` files. `stack ghci` would find the file and load it if I started it in VSCode terminal window, or I could `:l filename.hs` and it would load. Then `:r` reloads easy enough. But then I needed the `QuasiQuoter` package or some equiv to do multi line strings. I had to create a full `stack` project which could be done with `stack init` in order to create the dependency (effectively do the equiv of `yarn add`). All in all, the `stack` system is not so great. JS's `npm` / `yarn`, `dotnet`, `cargo`, are all better.

Once that was done, could dump in multi line strings. Created a module for all the input data. VSCode doesn't fold source code, so I didn't want 100s of lines of code from arrays of numbers in my solution file.

The actual problem - Day 01 is pretty easy. Only thing that caused me some static was the difference between `div` and `quot` for integer division. `quot` truncates toward zero, and `div` truncates toward negative infinity. The difference can cause a slight error which the problem is clearly designed to expose. Consider yourself warned :)

Part 2 of the problem, which I did with a fold in F#, seemed more natural as recursion in Haskell:

```Haskell
deepFuelReq :: Int -> Int
deepFuelReq n | n <= 0 = 0
deepFuelReq n = (if req < 0 then 0 else req) + deepFuelReq req
    where req = fuelReq n

part2 = sum (map deepFuelReq amounts)
```
Could probably get tighter rather than having that `if` stuck in the middle, but this worked and was clear to me.

### [Day 02](https://adventofcode.com/2019/day/2) Part 1
The only two interesting things here were finding and using the `splitAt` function from the `Data.List` package, I could have wrote that myself but I didn't feel like it. And the run function which I made two versions of. The first version whas a bit shorter by line count, but required repeating the recursive call twice which was aesthetically unpleasant. Went with the following. Originally the clause `(replace pAns...)` was factored out into a separate `apply` function defined in the `where` clause, but that was unneeded. I like that case can be one line if the cases are short, like here.

```Haskell
run :: Int -> [Int] -> [Int]
run pos mem =
    if op /= 99 
    then run (pos + 4) (replace pAns (fn (mem!!p1) (mem!!p2)) mem) 
    else mem
    where
        (op:p1:p2:pAns:_) = drop pos mem
        fn = case op of 1 -> (+); 2 -> (*); 99 -> \ _ _ -> 0
```
I'm using the `where` clause a fair amount. Not sure if I'm missing a more idomatic approch to solving these problems, time will tell.

## [Day 02](https://adventofcode.com/2019/day/2) Part 2
Again, pretty easy, needed to run through a comprehention to generate the possible inputs. Those were run through a recursive function that will short circuit out when it finds the solution:

```Haskell
solve :: [(Int,Int)] -> Int -> Maybe (Int,Int)
solve [] _ = Nothing
solve ((v1,v2):xs) target =
    if target == head (run 0 $ initMem v1 v2)
    then Just (v1, v2)
    else solve xs target
    
day2part2 = case solve [(x,y) | x <- [0..99], y <- [0..99]] target of
            Just (v1, v2) -> Just (100 * v1 + v2)
            Nothing -> Nothing
```

### Sidebar - Tooling
The first day or two, I wasn't that thrilled with [Stack](https://docs.haskellstack.org/en/stable/GUIDE) as a configuration, package manager, and build tool. Since then I've gotten comfortable enough with stack that starting new projects, adjusting references and dependencies, and especially the REPL to start really enjoying Haskell.

F# has a REPL as well as you know (as do many other languages, but I have most experience with F#), but the GHCI repl is more full featured and more fun to use. The `:type` and `:info` commands are especially useful for interrogating type and typeclass information, and the workflow with the `:r` reload makes iterating fun and fast.

Integration with VSCode has proven nice as well, although I find I have to kill the language server once in a while if I get it into a f'd up state. I leave activity monitor open and filtered to `hie`, and if there's an issue I kill `hie-8.6.5`. It restarts automatically after a second or two and usually is back in order.

## [Day 03](https://adventofcode.com/2019/day/3)

Day 3 part 1 was the last day that I did in F# before starting on the Haskell path with Advent of code. Neither half was too bad especially because I had the F# version done. In this day I started getting more comfortable with the `let .. in` and `where` clauses in a function definition. Haskell is pretty strict about a function being a single expression, so you have to use the `let .. in` and `where` if you want to bind intermediate values on the way to the final solution like this:

```Haskell
findSigDelay raw =
    "Minimum combined lengths: " ++ show minCombLens ++ "\n"
  where
    [p1,p2] = findPaths raw
    board = M.fromList p1
    ints = [((x,y), t2) | ((x,y),t2) <- p2, M.member (x,y) board] -- gets you the intersections and p2 distances
    combinedLengths = map (\(loc, t2) -> 
      let t1 = board M.! loc  in
      t1 + t2) ints
    minCombLens = minimum combinedLengths
```

This is a bit different from F#, which gives you some sugar, allowing binding of locals throughout a function with the last expression being returned. This is what's happening with `where` except you have to be explicit. I'm comfortable with this syntax, it adds to the feel that a functions are more functions in the mathematical sense than in the subroutine like, side effect causing way we're used to.

`let..in` binding I use less often as it's more cumbersome if you have more than one or two bindings to create. It was the `let..in` approach that F# used in its very early days while it was still a research project at MS Research, until they added the more terse, sugared syntax.

## [Day 04](https://adventofcode.com/2019/day/4)
*written post-dated after day 10*
Day 4 was pretty easy as well. Turning the number into a list of digits allowed for simple manipulation. Getting more comfortable with both recursion and gates. 

```Haskell
-- | first solution, rising sequnce of digits (via fixup), valid has at least one
-- occurence of at least 2 sequential digits
proc1 :: Int -> Int -> [Int] -> [Int]
proc1 mx n c
    | n' > mx = c
    | isValid = proc1 mx (n'+1) (n':c)
    | otherwise = proc1 mx (n'+1) c
  where
    check = fixup $ digs n
    n' = undigs check
    isValid = (maximum . map length . group $ check) >= 2
```
Combined with the `where` clause, using gates feels very expressive and to the point, with almost no added effort. I find myself referring back to my own code for the details of the syntax, but that's just because it's still new; these things will become muscle memory soon enough.

Like when using gates, recursion evokes similar feelings for me in terms of the layout, mostly because of the lack of the familiar function declaration and body layout found almost everywhere else; both gated functions and recursive functions have unique layouts in comparison, though both have completely different capabilities and meanings as you know.

```Haskell
-- | Assures that each digit in a list is equal or greater than the previous
fixup :: [Int] -> [Int]
fixup [] = []
fixup [n] = [n]
fixup (n1:n2:n3:rest) | n2 < n1 = n1 : fixup (n1:n1:rest)
fixup [n1,n2] | n2 < n1 = [n1,n1] 
fixup (n1:rest) = n1 : fixup rest
```

F# of course allows recursion, but it often feels like an antipattern if there's any other way to get the job done. Haskell feels much more welcoming wrt recursion.

## [Day 05](https://adventofcode.com/2019/day/4)
_I've revisited day 5 a couple of times as required by subsequent days_

Day 5 is when it starts getting really fun. The IntCode computer from day 2 starts getting enhanced. This happens here and also subsequent days. The first enhancement is adding opcodes for input and output. I completely start over from `Day02.hs` with `Day05.hs` The first difference is that we have more opcodes:

```Haskell
data Op = Add | Mult | Inp | Outp
        deriving (Show, Enum, Eq)

-- | From an integer that indicates an opcode, get the opcode and the number of parameters expected
opOfInt n = case n of 1 -> (Add,2) 
                        2 -> (Mult,2)
                        3 -> (Inp,1) 
                        4 -> (Outp,1)
```
Now in addition to add and multiply, the computer can retrieve input from some imaginary stream one `Int` at a time, and similarly write output to some imaginary out stream. `opOfInt` translates the opcode from memory into the `Op` type, along with an integer indicating how far to move the instruction pointer after execution.

The state of the computer is maintained in this record type (aka Sum type):

```Haskell
data World = World { ins     :: [Int]
                    , outs   :: [Int]
                    , mem    :: Memory
                    , offset :: Int
                    } deriving (Show)

```
`[Int]` implement the input and output streams, `Memory` is an alias for `[Int]` which comes in handly later when we change memory implementation. Besides that all that's left is the offset, which points to the next instruction to be executed. The `run` function recursively evaluates the next statement against the state of the world, getting a new world back, and a directive to either halt or continue. 

```Haskell
    run :: World -> World
    run world =
        case eval world (addrToStatement world) of
            (world', True) -> run world'
            (world', False) -> world
```

`eval` is now a case function that delegates to the code to run particular opcodes (Note that Day 5 part 2 added even more opcodes for jumping to addresses, and doing boolean comparisons for equals and less than).

```Haskell
-- | select appropriate operation evaluator
eval :: World -> Statement -> (World, Bool)
eval world (Add,  params)          = (binOp world Add  params, True)
eval world (Mult, params)          = (binOp world Mult params, True)
eval world (Inp,  ((Pos,addr):[])) = (readOp world addr, True)
eval world (Outp, (param:[]))      = (writeOp world param, True)        
eval world (JumpF,params)          = (jumpOp world JumpF params, True)
eval world (JumpT,params)          = (jumpOp world JumpT params, True)
eval world (Equals,params)         = (boolOp world Equals params, True)
eval world (LessThan,params)       = (boolOp world LessThan params, True)    
eval world (Terminate, _)          = (world, False)
```

Each operation evaluation function (like `binOp`, `readOp`, `writeOp`, etc), does its work, and determines if the computer should halt or continue. For example `binOp` handles the `Add` and `Mult` operations:

```Haskell
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
```

There's more to it - check the source. Working on the IntCode computer is a lot of fun and I'm enjoying it when we're asked to return to it an enhance it. This happens in Day 7 and Day 9 (writing this after Day 10) again so far, I'm sure they'll continue to revisit it.


## Day 07 Part 2
Missed several days of not taking. I'll catch up soon...
Day 07 part 2 was tough - I made it tougher than it had to be - dumb mistake, not reading carefully enough. Oh well lesson learned. More notes soon...

## References
* [Stack](https://docs.haskellstack.org/en/stable/GUIDE) - build system & all around `nvm` like capabilities ... read the docs to get the whole picture.
* Using [QuasiQuoter](http://hackage.haskell.org/package/string-qq) ([Source](git://github.com/audreyt/string-qq)) to allow multi-line strings. I'm just learning Haskell, but it appears that language extensions / macros are possible, and that's how this thing is made to work.

**Note to self**: Haskel Advent of Code 2019 is under my twitter login, and the F# version is under my Google login.