getInput :: IO _
getInput = do
    inLines <- lines <$> readFile "input.txt"
    return $ parseInput inLines
  where
    parseInput inLines =
        undefined

problem1 :: IO ()
problem1 = do
    input <- getInput
    print $ solve input
  where
    solve input =
        undefined :: String

problem2 :: IO ()
problem2 = do
    input <- getInput
    print $ solve input
  where
    solve input =
        undefined :: String

-- Utils --

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn needle inList =
    go [] [] inList
  where
    go acc curList (x:xs) =
        if x == needle
        then go (reverse curList:acc) [] xs
        else go acc (x:curList) xs
    go acc curList [] =
        reverse $ reverse curList:acc
