import Control.Monad (guard)

import Data.Char (isDigit, isAlpha)
import qualified Data.Maybe as Maybe
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

-- Map[outerBagColor -> Map[innerBagColor -> numInnerBag]]
type BagMap = Map String (Map String Int)

getInput :: IO BagMap
getInput = do
    inLines <- lines <$> readFile "input.txt"
    return $ parseInput inLines
  where
    parseInput inLines =
        Map.fromList $ fmap parseLine inLines

    parseLine ln =
        let
            cleanedLine = filter (/='.') ln -- "light red bags contain 1 bright white bag, 2 muted yellow bags"
            outerBag = unwords $ takeWhile (/="bags") (words cleanedLine) -- "light red"
            innerBagsString = dropWhile (not . isDigit) cleanedLine -- "1 bright white bag, 2 muted yellow bags"
            innerBags =
                if null innerBagsString -- i.e. "no other bags"
                then []
                else
                    fmap (\str -> -- " 2 muted yellow bags"
                        ( unwords -- "muted yellow"
                            $ takeWhile (\word -> word /= "bag" && word /= "bags") -- ["muted", "yellow"]
                            $ words -- ["muted", "yellow", "bags"]
                            $ dropWhile (not . isAlpha) str -- "muted yellow bags"
                        , read (takeWhile (not . isAlpha) str) -- 2
                        )
                    ) (splitOn ',' innerBagsString)
        in
        (outerBag, Map.fromList innerBags)


problem1 :: IO ()
problem1 = do
    input <- getInput
    print $ solve input
  where
    solve input =
        length $ filter (bagContains input "shiny gold") (Map.keys input)

    bagContains :: BagMap -> String -> String -> Bool
    bagContains bagMap searchColor containerColor = Maybe.fromMaybe False $ do
        innerBags <- Map.lookup containerColor bagMap
        guard $ (not . null) (Map.toList innerBags)
        return $ Map.member searchColor innerBags
              || any (bagContains bagMap searchColor) (Map.keys innerBags)

problem2 :: IO ()
problem2 = do
    input <- getInput
    print $ solve input
  where
    solve input =
        countInnerBags input "shiny gold"

    countInnerBags :: BagMap -> String -> Int
    countInnerBags bagMap bagColor = Maybe.fromMaybe 0 $ do
        innerBags <- Map.toList <$> Map.lookup bagColor bagMap
        return $ sum $ fmap (\(innerBag, numberOfBag) ->
                (1 + countInnerBags bagMap innerBag) * numberOfBag
            ) innerBags

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
