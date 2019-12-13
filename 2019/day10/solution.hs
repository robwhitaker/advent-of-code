module Solution where

import Data.Maybe (mapMaybe)
import Data.List (groupBy, sort, span)
import Control.Monad (void)

type Point = (Float, Float)
type Angle = Float
type Distance = Float

type RelativePoint = (Angle, Distance, Point)

getInput :: String -> IO [Point]
getInput file = do
    inputLines <- lines <$> readFile file
    return
        $ concat
        $ map (\(y, xs) ->
                mapMaybe (\(x, ch) ->
                    case ch of
                        '.' -> Nothing

                        '#' -> Just (x,y)
                ) (zip [0..] xs)
            ) (zip [0..] inputLines)

angleBetween :: Point -> Point -> Float
angleBetween (x1, y1) (x2, y2) =
    atan2 (y1 - y2) (x1 - x2)

distanceBetween :: Point -> Point -> Float
distanceBetween (x1, y1) (x2, y2) =
    sqrt $ (x2 - x1) ** 2 + (y2 - y1) ** 2

pointToPoints :: Point -> [Point] -> [RelativePoint]
pointToPoints origin ps =
    mapMaybe (\p ->
        if p /= origin
           then Just (angleBetween origin p, distanceBetween origin p, p)
           else Nothing
    ) ps

sortAndGroupByAngle :: [RelativePoint] -> [[RelativePoint]]
sortAndGroupByAngle = groupBy (\(a1,_,_) (a2,_,_) -> a1 == a2) . sort

problem1 :: IO ()
problem1 = do
    points <- getInput "input.txt"
    print
        $ maximum
        $ map (\point ->
                ( length
                    $ map head
                    $ sortAndGroupByAngle
                    $ pointToPoints point points
                , point
                )
            ) points

problem2 :: IO ()
problem2 = do
    relativePoints <- problem2' "input.txt" (17, 22)
    let (_, _, (x, y)) = relativePoints !! 199
    print $ round $ x * 100 + y

problem2' :: String -> Point -> IO [RelativePoint]
problem2' file station = do
    points <- getInput file
    let startAngle = angleBetween (1,1) (1,0) -- 90 degrees in radians
        relativePoints = sortAndGroupByAngle $ pointToPoints station points
        (skip, start) = span ((\(angle,_,_) -> angle < startAngle) . head) relativePoints
        pointsStartingAt90 = start <> skip
    return $ extendPoints (length pointsStartingAt90) pointsStartingAt90 pointsStartingAt90

extendPoints :: Int -> [[RelativePoint]] -> [[RelativePoint]] -> [RelativePoint]
extendPoints currentLength acc remainingPoints
    | remainingPoints == [] = fmap head acc
    | otherwise =
        let nextExt = mapMaybe (\pGroup ->
                    case pGroup of
                        (x:[]) -> Nothing
                        (x:xs) -> Just xs
                ) remainingPoints
        in
        extendPoints (currentLength + length nextExt) (acc <> nextExt) nextExt
