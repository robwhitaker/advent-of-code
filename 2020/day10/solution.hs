import qualified Data.List as List

getInput :: IO [Int]
getInput = fmap read . lines <$> readFile "input.txt"

problem1 :: IO ()
problem1 = do
    input <- getInput
    print $ solve input
  where
    solve input =
        let adapters = List.sort input
            deviceJoltage = maximum adapters + 3
            chain = 0 : adapters <> [deviceJoltage]
            diffs = zipWith (-) (drop 1 chain) chain
        in
        product $ fmap length $ List.group $ List.sort $ filter (/=2) diffs

problem2 :: IO ()
problem2 = do
    input <- getInput
    print $ solve input
  where
    solve input =
        let deviceJoltage = maximum input + 3
            chain = reverse $ 0 : List.sort input
        in
        countPaths chain [(deviceJoltage, 1)]

    countPaths :: [Int] -> [(Int, Int)] -> Int
    countPaths chain initialHistory =
        snd $ head $ foldl (\numHistory currentNum ->
            let
                pathsFromX = sum $ fmap snd $ takeWhile ((<=(currentNum+3)) . fst) numHistory
            in
            (currentNum, pathsFromX) : numHistory
        ) initialHistory chain
