import qualified Data.Set as Set
import qualified Data.List as List

type Response = String

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

getInput :: IO [[Response]]
getInput =
    splitOn "" . lines <$> readFile "input.txt"

problem1 :: IO ()
problem1 = do
    responseGroups <- getInput
    print $ sum $ fmap (Set.size . Set.fromList . mconcat) responseGroups

problem2 :: IO ()
problem2 = do
    responseGroups <- getInput
    print $ sum $ fmap getNumCommonResponses responseGroups
  where
    getNumCommonResponses responses =
        let groupedResponses = List.group $ List.sort $ mconcat responses
            numGroupMembers = length responses
            unanimousResponses = filter ((==numGroupMembers) . length) groupedResponses
        in
        length unanimousResponses
