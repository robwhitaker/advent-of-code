import Data.Char (isDigit)

getInput :: IO [(Int, Int, Char, String)]
getInput = do
    lines <- lines <$> readFile "input.txt"
    return $ fmap parseLine lines
  where
    parseLine str =
        let [nums, char:":", password] = words str
            minNum = read $ takeWhile isDigit nums
            maxNum = read $ drop 1 $ dropWhile (/='-') nums
        in
        (minNum, maxNum, char, password)

problem1 :: IO ()
problem1 = do
    passwords <- getInput
    print $ length $ filter validate passwords
  where
    validate (minNum, maxNum, char, password) =
        let
            num = length $ filter (==char) password
        in
        num >= minNum && num <= maxNum

problem2 :: IO ()
problem2 = do
    passwords <- getInput
    print $ length $ filter validate passwords
  where
    validate (pos1, pos2, char, password) =
        let
            posMatches = [ password !! (pos1-1) == char, password !! (pos2-1) == char ]
        in
        length (filter id posMatches) == 1
