import           System.Environment (getArgs)

import           Control.Monad.State (State)
import qualified Control.Monad.State as State

import qualified Data.Maybe as Maybe

data Input = Input
    { currentTime :: Int
    , busIds :: [Maybe Int]
    }

getInput :: IO Input
getInput = do
    inLines <- lines <$> readFile "input.txt"
    return $ parseInput inLines
  where
    parseInput inLines =
        let
            [currentTimeStr, busIdsStr] = inLines
            busIds =
                fmap (\idStr ->
                    if idStr == "x" then Nothing else Just (read idStr)
                ) $ splitOn ',' busIdsStr
        in
        Input (read currentTimeStr) busIds


problem1 :: IO ()
problem1 = do
    input <- getInput
    print $ solve input
  where
    solve input =
        let
            time = currentTime input
            busses = Maybe.mapMaybe id (busIds input)
            nextDepartures =
                filter (not . null . snd)
                $ fmap (\t -> (t, filter (departsAt t) busses)) [time..]
            -- Normally a very unsafe pattern match, but the problem guarantees
            -- there will only be one answer
            (departTime, [departBusId]) = head nextDepartures
        in
        (departTime - time) * departBusId


    departsAt :: Int -> Int -> Bool
    departsAt time busId =
        time `mod` busId == 0

{- Problem 2, having such a large result, requires a much more efficient solution
   than simply testing every number until we get the right one. Due to the cyclic
   nature of the bus schedules, turning it into a series of mod equations seemed
   the most straightforward mathematical representation. With the example input:

   7,13,x,x,59,x,31,19

   we can say:

   x     `mod`  7 == 0
   (x+1) `mod` 13 == 0
   (x+4) `mod` 59 == 0
   (x+6) `mod` 31 == 0
   (x+7) `mod` 19 == 0

   where x is the departure time(s) we are looking for.

   We want to solve for x, so we want to get x on its own in the mod function. Knowing,
   for example, that (x+4) `mod` 59 == 0, we can say that x `mod` 59 == 59-4 or 55.
   Therefore:

   x `mod`  7           == 0
   x `mod` 13 == 13 - 1 == 12
   x `mod` 59 == 59 - 4 == 55
   x `mod` 31 == 31 - 6 == 25
   x `mod` 19 == 19 - 7 == 12

   Or, in more mathetical terms:

   x ≡ 0  (mod  7)
   x ≡ 12 (mod 13)
   x ≡ 55 (mod 59)
   x ≡ 25 (mod 31)
   x ≡ 12 (mod 19)

   From there, we can use the Chinese Remainder Theorem to get our answer:
   https://www.youtube.com/watch?v=zIFehsBHB8o

   NOTE: Using this theorem relies on the assumption that all of the input numbers are
         pairwise coprime. While this is indeed the case for both the example numbers
         and my own input, the problem does not state that this is a guarantee! Mileage
         may vary.
-}

data ModRow = ModRow
    { b :: Int
    , n :: Int
    , x :: Int
    }

-- Placeholder for x's which does not effect
-- the outcome of the computation
nullRow :: ModRow
nullRow = ModRow 0 0 0

problem2 :: IO ()
problem2 = do
    input <- getInput
    print $ solve input
  where
    solve input =
        let
            _N = product $ Maybe.mapMaybe id (busIds input)
            modRows = State.evalState (mapM (mkModRow _N) (busIds input)) 0
        in
        sum (fmap modRowProduct modRows) `mod` _N

    modRowProduct :: ModRow -> Int
    modRowProduct (ModRow b n x) =
        b * n * x

    mkModRow :: Int -> Maybe Int -> State Int ModRow
    mkModRow _ Nothing =
        State.modify (+1) >> return nullRow
    mkModRow _N (Just busId) = do
        offset <- State.get
        State.modify (+1)
        let
            theB = busId - offset
            theN = _N `div` busId
            theX = findX busId theN
        return $ ModRow theB theN theX
      where
        findX theMod theN =
            head
            $ dropWhile (\x -> (theN `mod` theMod) * x `mod` theMod /= 1)
                        [1..]

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

-- Main --

main :: IO ()
main = do
    [problem] <- getArgs
    case problem of
        "1" -> problem1
        "2" -> problem2
