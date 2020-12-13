import qualified Data.Set as Set

type Op = Range -> Range
type Range = (Int, Int)
type Seat = (Int, Int)

firstHalf :: Op
firstHalf (low, high) =
    (low, low + (high - low) `div` 2)

secondHalf :: Op
secondHalf (low, high) =
    (low + ceiling (fromIntegral (high - low) / 2), high)

toRowOp :: Char -> Op
toRowOp 'F' = firstHalf
toRowOp 'B' = secondHalf
toRowOp _   = error "Invalid row char."

toColOp :: Char -> Op
toColOp 'L' = firstHalf
toColOp 'R' = secondHalf
toColOp _   = error "Invalid col char."

stringToSeat :: String -> Seat
stringToSeat inStr =
    let
        row = snd $ foldl (flip ($)) (0, 127) rowOps
        col = snd $ foldl (flip ($)) (0,   7) colOps
    in
    (row, col)
  where
    rowOps = fmap toRowOp $ take 7 inStr
    colOps = fmap toColOp $ drop 7 inStr

seatId :: Seat -> Int
seatId (row, col) =
    row*8 + col

getInput :: IO [Seat]
getInput =
    fmap stringToSeat . lines <$> readFile "input.txt"

problem1 :: IO ()
problem1 = do
    seats <- getInput
    print $ maximum $ fmap seatId seats

problem2 :: IO ()
problem2 = do
    seats <- getInput
    let seatIds = Set.fromList $ fmap seatId seats
    print $ head
        [ seatId (row, col)
        | row <- [0..127]
        , col <- [0..7]
        , Set.notMember (seatId (row, col)) seatIds
        , Set.member (seatId (row, col) + 1) seatIds
        , Set.member (seatId (row, col) - 1) seatIds
        ]
