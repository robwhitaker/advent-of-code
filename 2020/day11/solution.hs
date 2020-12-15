import           System.Environment (getArgs)
import           Control.Monad (guard)

import qualified Data.Maybe as Maybe
import           Data.Array.IArray (Array, (!))
import qualified Data.Array.IArray as Array

data SeatStatus = Floor | Empty | Occupied deriving (Show, Eq)

type Position = (Int, Int) -- row, column

data Seat = Seat
    { status :: SeatStatus
    , neighbors :: [Position]
    } deriving (Show, Eq)

seatStatus :: Char -> SeatStatus
seatStatus '.' = Floor
seatStatus 'L' = Empty
seatStatus '#' = Occupied
seatStatus _   = error "Invalid character."

type Seats = Array Position Seat

-- Helpful for debugging
printSeats :: Seats -> IO ()
printSeats seats = do
    mapM_ (\((row, col), seat) ->
            if col == 0
            then do putStrLn ""
                    putStr (seatToString seat)
            else putStr (seatToString seat)
        ) (Array.assocs seats)
    putStrLn ""
  where
    seatToString seat =
        case status seat of
            Floor -> "."
            Empty -> "L"
            Occupied -> "#"

-- Main program

getInput :: (Array Position SeatStatus -> Position -> [Position]) -> IO Seats
getInput findNeighbors = do
    inLines <- lines <$> readFile "input.txt"
    return $ parseInput inLines
  where
    parseInput :: [String] -> Seats
    parseInput inLines =
        let cols = length (head inLines)
            rows = length inLines
            seatList = fmap seatStatus (mconcat inLines)
            seatStatusByPosition =
                Array.listArray ((0,0), (rows-1,cols-1)) seatList
            seatList2dIndexed =
                zipWith (\pos status ->
                    Seat status (findNeighbors seatStatusByPosition pos)
                ) [ (n `div` cols, n `mod` cols) | n <- [0..] ] seatList
        in
        Array.listArray ((0,0), (rows-1,cols-1)) seatList2dIndexed

safeGet :: Array Position a -> Position -> Maybe a
safeGet seats ix@(row, col) = do
    let ((lowerRow, lowerCol), (higherRow, higherCol)) = Array.bounds seats
    guard $ row >= lowerRow && row <= higherRow && col >= lowerCol && col <= higherCol
    return $ seats ! ix

stepSeat :: Seats -> Int -> Seat -> Seat
stepSeat seats abandonSeatThreshold seat =
    let
        surrounding = Maybe.mapMaybe (safeGet seats) (neighbors seat)
        numOccupied = length $ filter ((==Occupied) . status) surrounding
    in
    if status seat == Empty && numOccupied == 0
    then seat { status = Occupied }
    else if status seat == Occupied && numOccupied >= abandonSeatThreshold
    then seat { status = Empty }
    else seat

step :: Seats -> Int -> Seats
step seats abandonSeatThreshold =
    Array.amap (stepSeat seats abandonSeatThreshold) seats

stepUntilDone :: Seats -> Int -> Seats
stepUntilDone seats abandonSeatThreshold =
    let
        stepped = step seats abandonSeatThreshold
    in
    if stepped == seats
    then seats
    else stepUntilDone stepped abandonSeatThreshold

problem1 :: IO ()
problem1 = do
    input <- getInput seatsAround
    print $ solve input
  where
    solve input =
        length
        $ filter ((==Occupied) . status)
        $ Array.elems
        $ stepUntilDone input 4

    seatsAround :: Array Position SeatStatus -> Position -> [Position]
    seatsAround seats (row, col) =
        [ (r, c)
        | r <- [row+1,row-1,row]
        , c <- [col+1,col-1,col]
        , (r, c) /= (row, col)
        ]

problem2 :: IO ()
problem2 = do
    input <- getInput findNeighbors
    print $ solve input
  where
    solve input =
        length
        $ filter ((==Occupied) . status)
        $ Array.elems
        $ stepUntilDone input 5

    findNeighbors :: Array Position SeatStatus -> Position -> [Position]
    findNeighbors seats currentPos@(currentRow, currentCol) =
        Maybe.mapMaybe id
            [ getNextSeat 0 1
            , getNextSeat 1 0
            , getNextSeat 0 (-1)
            , getNextSeat (-1) 0
            , getNextSeat 1 1
            , getNextSeat 1 (-1)
            , getNextSeat (-1) 1
            , getNextSeat (-1) (-1)
            ]
      where
        getNextSeat rowDelta colDelta =
            let
                nextSeatPositions =
                    zip [ currentRow,currentRow+rowDelta..]
                        [ currentCol,currentCol+colDelta..]
            in
            (\pos -> const pos <$> safeGet seats pos)
            $ head
            $ dropWhile ((==Just Floor) . safeGet seats)
            $ drop 1 nextSeatPositions

main :: IO ()
main = do
    [problem] <- getArgs
    case problem of
        "1" -> problem1
        "2" -> problem2
