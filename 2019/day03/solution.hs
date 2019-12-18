import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (sort, groupBy)

type Move = (Int, Int)
type Pos = (Int, Int)

split :: Char -> String -> [String]
split sep =
    filter (/=[sep])
    . groupBy (\a b -> not $ sep `elem` [a,b])

movesFromString :: String -> [Move]
movesFromString =
    map toMove . split ','
  where
      toMove ('U':n) = (0, read n)
      toMove ('D':n) = (0, -1 * read n)
      toMove ('L':n) = (-1 * read n, 0)
      toMove ('R':n) = (read n, 0)

positionsFromMoves :: [Move] -> Map Pos Int
positionsFromMoves =
    (\(_,_,newMap) -> newMap)
    . foldl (\acc@((posX, posY), step, posMap) (moveX, moveY) ->
        let xs =
                if moveX > 0 then
                    [posX+1..posX+moveX]
                else if moveX < 0 then
                    [posX-1,posX-2..posX+moveX]
                else
                    [posX]
            ys =
                if moveY > 0 then
                    [posY+1..posY+moveY]
                else if moveY < 0 then
                    [posY-1,posY-2..posY+moveY]
                else
                    [posY]
            newPositions = [ (x, y) | x <- xs, y <- ys ]
            (newStep, newMap) =
                foldl (\(step', posMap') pos ->
                    ( step' + 1
                    , if Map.member pos posMap'
                         then posMap'
                         else Map.insert pos (step'+1) posMap'
                    )
                ) (step, posMap) newPositions
            newPos = (posX + moveX, posY + moveY)
        in
        (newPos, newStep, newMap)
    ) ((0, 0), 0, Map.empty)

distance :: Move -> Int
distance (x, y) = abs x + abs y

problem1 :: IO ()
problem1 = do
    input <- readFile "input.txt"
    let [ wire1, wire2 ] = map (Map.keysSet . positionsFromMoves . movesFromString) $ lines input
        inBothSets = Set.intersection wire1 wire2
        distances = sort $ map distance $ Set.toList inBothSets
    print $ head distances

problem2 :: IO ()
problem2 = do
    input <- readFile "input.txt"
    let [ wire1, wire2 ] = map (positionsFromMoves . movesFromString) $ lines input
        inBothMaps = Map.filterWithKey (\k _ -> k /= (0, 0)) $ Map.intersectionWith (+) wire1 wire2
        steps = sort $ Map.elems inBothMaps
    print $ head steps
