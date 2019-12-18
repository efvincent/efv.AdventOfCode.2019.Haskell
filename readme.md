# Advent of Code 2019
Use Advent as exercises while learning Haskell.

**Note**: Haskel Advent of Code 2019 is under my twitter login, and the F# version is under my Google login.

## Notes / Experiences
Since this is a Haskell Learning experience, I'm going to do a little bit of a diary on how each exercise went.

### Day 01
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

### Day 02 Part 1
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

## Day 02 Part 2
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


## References
* [Stack](https://docs.haskellstack.org/en/stable/GUIDE) - build system & all around `nvm` like capabilities ... read the docs to get the whole picture.
* Using [QuasiQuoter](http://hackage.haskell.org/package/string-qq) ([Source](git://github.com/audreyt/string-qq)) to allow multi-line strings. I'm just learning Haskell, but it appears that language extensions / macros are possible, and that's how this thing is made to work.
