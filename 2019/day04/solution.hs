import Data.List (group, sort)

-- input: 359282-820401
range :: [Int]
range = [359282..820401]

hasNDuplicates :: (Int -> Bool) -> [Char] -> Bool
hasNDuplicates pred = any (pred . length) . group

isAscending :: Ord a => [a] -> Bool
isAscending xs = xs == sort xs

problem1 :: IO ()
problem1 = do
    let isValid numStr = isAscending numStr && hasNDuplicates (>1) numStr
    print $ length $ filter (isValid . show) range

problem2 :: IO ()
problem2 = do
    let isValid numStr = isAscending numStr && hasNDuplicates (==2) numStr
    print $ length $ filter (isValid . show) range
