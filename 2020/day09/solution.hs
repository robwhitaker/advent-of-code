getInput :: IO [Int]
getInput = fmap read . lines <$> readFile "input.txt"

findInvalid :: [Int] -> Int
findInvalid input =
    findNonSum (take 25 input) (drop 25 input)
  where
    findNonSum :: [Int] -> [Int] -> Int
    findNonSum prev25 (h:rest) =
        if null [ () | x <- prev25, y <- prev25, x /= y, x + y == h ]
        then h
        else findNonSum (drop 1 $ prev25 <> [h]) rest


problem1 :: IO ()
problem1 = do
    input <- getInput
    print $ findInvalid input

problem2 :: IO ()
problem2 = do
    input <- getInput
    print $ solve input
  where
    solve input =
        let invalid = findInvalid input
            contiguousNums = findContiguous invalid invalid [] input
        in
        minimum contiguousNums + maximum contiguousNums

findContiguous :: Int -> Int -> [Int] -> [Int] -> [Int]
findContiguous initial remaining currentSet (h:rest) =
    if h >= initial
        then findContiguous initial initial [] rest
    else if remaining - h == 0
        then currentSet <> [h]
    else if remaining - h < 0
        then findContiguous initial (remaining + head currentSet) (drop 1 currentSet) (h:rest)
    else
        findContiguous initial (remaining - h) (currentSet <> [h]) rest
