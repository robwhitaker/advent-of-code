import           Data.Char (isDigit)
import qualified Data.Map.Strict as Map
import           Text.Read (readMaybe)
import           Control.Monad (guard)
import qualified Data.Maybe as Maybe

data Height = Height
    { value :: Int
    , units :: String
    } deriving (Eq)

data Passport = Passport
    { birthYear :: Int
    , issueYear :: Int
    , expirationYear :: Int
    , height :: Height
    , hairColor :: String
    , eyeColor :: String
    , passportId :: String
    , countryId :: Maybe String
    } deriving (Eq)

parseYear :: Int -> Int -> String -> Maybe Int
parseYear minYear maxYear inStr = do
    year <- readMaybe inStr
    guard (year <= maxYear && year >= minYear)
    return year

parseHeight :: String -> Maybe Height
parseHeight str = do
    value <- readMaybe $ takeWhile isDigit str
    let unit = dropWhile isDigit str
    case unit of
        "cm" -> guard (value >= 150 && value <= 193)
        "in" -> guard (value >= 59 && value <= 76)
        _    -> guard False
    return $ Height value unit

validateHairColor :: String -> Maybe String
validateHairColor color = do
    guard (length color == 7)
    guard (head color == '#')
    guard (all validChar (tail color))
    return color
  where
    validChar = (`elem` (['0'..'9'] <> ['a'..'f']))

validateEyeColor :: String -> Maybe String
validateEyeColor color = do
    guard $ color `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    return color

validatePassportId :: String -> Maybe String
validatePassportId pid = do
    guard $ length pid == 9 && all isDigit pid
    return pid

parsePassportFields :: String -> Map.Map String String
parsePassportFields =
    foldl (\fieldMap fieldStr ->
        Map.insert (take 3 fieldStr) (drop 4 fieldStr) fieldMap
    ) Map.empty
    . words

parsePassport :: String -> Maybe Passport
parsePassport str =
    Passport
        <$> (Map.lookup "byr" fields >>= parseYear 1920 2002)
        <*> (Map.lookup "iyr" fields >>= parseYear 2010 2020)
        <*> (Map.lookup "eyr" fields >>= parseYear 2020 2030)
        <*> (Map.lookup "hgt" fields >>= parseHeight)
        <*> (Map.lookup "hcl" fields >>= validateHairColor)
        <*> (Map.lookup "ecl" fields >>= validateEyeColor)
        <*> (Map.lookup "pid" fields >>= validatePassportId)
        <*> Just (Map.lookup "cid" fields)
  where
    fields = parsePassportFields str

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

getInputAsFields :: IO [Map.Map String String]
getInputAsFields = do
    inLines <- lines <$> readFile "input.txt"
    return $ fmap parsePassportFields $ fmap unwords $ splitOn "" inLines

getInput :: IO [Maybe Passport]
getInput = do
    inLines <- lines <$> readFile "input.txt"
    return $ fmap parsePassport $ fmap unwords $ splitOn "" inLines

problem1 :: IO ()
problem1 = do
    passportsAsFields <- getInputAsFields
    print $ length $ Maybe.mapMaybe validate passportsAsFields
  where
    validate fields = do
        _ <- Map.lookup "byr" fields
        _ <- Map.lookup "iyr" fields
        _ <- Map.lookup "eyr" fields
        _ <- Map.lookup "hgt" fields
        _ <- Map.lookup "hcl" fields
        _ <- Map.lookup "ecl" fields
        _ <- Map.lookup "pid" fields
        return ()

problem2 :: IO ()
problem2 = do
    passports <- getInput
    print $ length $ filter (/=Nothing) passports
