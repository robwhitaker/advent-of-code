import Data.List (unfoldr)

getInput :: IO [Int]
getInput =
    readFile "input.txt" >>= return . map read . lines

calcFuel :: Int -> Int
calcFuel =
    subtract 2 . (`div` 3)

problem1 :: IO ()
problem1 =
    getInput >>= print . sum . map calcFuel

problem2 :: IO ()
problem2 =
    getInput >>= print . sum . concatMap calcFuelRec
  where
    calcFuelRec =
        unfoldr (\n ->
            let fuel = calcFuel n
            in
            if fuel > 0
               then Just (fuel, fuel)
               else Nothing
        )
