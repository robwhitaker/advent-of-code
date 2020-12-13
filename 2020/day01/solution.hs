import qualified Data.Set as Set
import qualified Data.List as List

getInput :: IO [Int]
getInput =
    readFile "input.txt" >>= return . fmap read . lines

problem1 :: IO ()
problem1 = do
    numbers <- getInput
    let
        result = foldl (\(answer, diffSet) n ->
                if Set.member n diffSet
                then ((2020-n) * n, diffSet)
                else (answer, Set.insert (2020-n) diffSet)
            ) (0, Set.empty) numbers
    print (fst result)

-- This could probably be way more efficient; as it is, it's something
-- gross like O(n^3).
problem2 :: IO ()
problem2 = do
    numbers <- getInput
    print $ solve $ List.sort numbers
  where
    solve numbers =
        let (x, y, z) = findSumGroup numbers
        in
        x * y * z

    findSumGroup :: [Int] -> (Int, Int, Int)
    findSumGroup (x:xs) =
        let nextPairs = mkPairs xs
            next2 = filter (\(y, z) -> x + y + z == 2020) nextPairs
        in case next2 of
               ((y, z):_) -> (x, y, z)
               _ -> findSumGroup xs

    mkPairs :: [Int] -> [(Int, Int)]
    mkPairs (x:xs) =
        fmap (\y -> (x, y)) xs <> mkPairs xs
    mkPairs [] = []
