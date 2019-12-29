# Advent of Code 2019
Use Advent as exercises while learning Haskell.

**Note**: Haskel Advent of Code 2019 is under my twitter login, and the F# version is under my Google login.

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
Again, pretty easy, needed to run through a comprehention to generate the possible inputs. Those were
run through a recursive function that will short circuit out when it finds the solution:

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

## Day 07 Part 2
Missed several days of not taking. I'll catch up soon...
Day 07 part 2 was tough - I made it tougher than it had to be - dumb mistake, not reading carefully enough. Oh well lesson learned. More notes soon...

## References
* [Stack](https://docs.haskellstack.org/en/stable/GUIDE) - build system & all around `nvm` like capabilities ... read the docs to get the whole picture.
* Using [QuasiQuoter](http://hackage.haskell.org/package/string-qq) ([Source](git://github.com/audreyt/string-qq)) to allow multi-line strings. I'm just learning Haskell, but it appears that language extensions / macros are possible, and that's how this thing is made to work.
