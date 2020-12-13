data Hill = Hill
    { hillWidth :: Int
    , hillHeight :: Int
    , hillMap :: [[Bool]] -- True for tree, False for no tree
    }

type Slope = (Int, Int)

getInput :: IO Hill
getInput = do
    inLines <- lines <$> readFile "input.txt"
    let width = length $ head inLines
        height = length inLines
        hMap = fmap (fmap (=='#')) inLines
    return $ Hill width height hMap

treesOnSlope :: Hill -> Slope -> Int
treesOnSlope (Hill hillWidth hillHeight hillMap) (xSlope, ySlope) =
    let
        onlyYsOnSlope =
            fmap snd
            $ filter (\(y,_) -> y `mod` ySlope == 0)
            $ zip [0..] hillMap
    in
    snd $ foldl (\(x, res) row ->
            if row !! (x `mod` hillWidth)
            then (x+xSlope, res+1)
            else (x+xSlope, res)
        ) (0, 0) onlyYsOnSlope


problem1 :: IO ()
problem1 = do
    hill <- getInput
    print $ treesOnSlope hill (3,1)

problem2 :: IO ()
problem2 = do
    hill <- getInput
    print $ product $ fmap (treesOnSlope hill) [(1,1),(3,1),(5,1),(7,1),(1,2)]
