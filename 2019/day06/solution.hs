import qualified Data.Map as Map
import Data.Map (Map)

getInput :: IO (Map String [String])
getInput = do
    input <- readFile "input.txt"
    return $ Map.fromListWith (<>) $ fmap split $ lines input
  where
    split str =
        let (orbited,(')':orbiter)) = break (==')') str
        in
        (orbited, [orbiter])

calcChecksum :: Map String [String] -> Int
calcChecksum dataMap =
    recurse 1 "COM"
  where
    recurse depth orbited =
        let orbiters = Map.findWithDefault [] orbited dataMap
        in
        depth * (length orbiters) + sum (map (recurse (depth+1)) orbiters)

findPathsToYouAndSanta :: Map String [String] -> ([String], [String])
findPathsToYouAndSanta dataMap =
    case recurse (Nothing, Nothing) [] "COM" of
        (Just youPath, Just santaPath) ->
            (reverse youPath, reverse santaPath)

        _ ->
            error "Could not find you and Santa."
  where
    recurse maybeYouAndSanta currentPath orbited =
        case maybeYouAndSanta of
            (Just you, Just santa) ->
                maybeYouAndSanta

            (maybeYou, maybeSanta) ->
                case orbited of
                    "YOU" -> (Just currentPath, maybeSanta)
                    "SAN" -> (maybeYou, Just currentPath)
                    _ ->
                        let orbiters = Map.findWithDefault [] orbited dataMap
                        in
                        foldl (\maybeYouAndSanta' orbiter ->
                            recurse maybeYouAndSanta' (orbited:currentPath) orbiter
                        ) maybeYouAndSanta orbiters

problem1 :: IO ()
problem1 =
    getInput >>= print . calcChecksum

problem2 :: IO ()
problem2 = do
    (pathToYou, pathToSanta) <- findPathsToYouAndSanta <$> getInput
    let commonPrefixLength = length $ takeWhile (uncurry (==)) $ zip pathToYou pathToSanta
    print $ length pathToYou + length pathToSanta - 2 * commonPrefixLength
