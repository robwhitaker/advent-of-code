import Data.Array

problem1 :: IO ()
problem1 = do
    input <- readFile "input.txt"
    let
        inputList = read ("[" <> input <> "]")
        inputArr  = listArray (0, length inputList - 1) inputList // [(1,12),(2,2)]
    print $ evalArr inputArr 0 ! 0

problem2 :: IO ()
problem2 = do
    input <- readFile "input.txt"
    let
        inputList = read ("[" <> input <> "]")
        inputArr  = listArray (0, length inputList - 1) inputList
        startingValues = [ (a,b) | a<-[0..99], b<-[0..99] ]
        (n, v) = tryUntilEquals inputArr startingValues
    print $ 100 * n + v
  where
    tryUntilEquals arr (x:xs) =
        if evalArr (arr // [(1, fst x), (2, snd x)]) 0 ! 0 == 19690720
            then x
            else tryUntilEquals arr xs
    tryUntilEquals _ [] =
        error "Out of combinations."

evalArr :: Array Int Int -> Int -> Array Int Int
evalArr arr ix
    | cVal == 99 =
        arr
    | cVal == 1 || cVal == 2 =
        let
            op = if cVal == 1 then (+) else (*)
            ix2 = arr ! (ix + 1)
            ix3 = arr ! (ix + 2)
            destIx = arr ! (ix + 3)
            v2 = arr ! ix2
            v3 = arr ! ix3
        in
        evalArr
            (arr // [(destIx, op v2 v3)])
            (ix + 4)
    | otherwise =
        error "Invalid code."
  where
      cVal = arr ! ix
