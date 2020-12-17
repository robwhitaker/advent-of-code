import System.Environment (getArgs)
import Prelude hiding (Right, Left)

type Coords = (Int, Int) -- East-West, North-South

data CompassDir = North | East | South | West deriving (Enum, Show)
data Turn = Left | Right deriving (Show)
data MoveType = MoveDir CompassDir | Turn Turn | MoveForward deriving (Show)
data Move = Move
    { moveType :: MoveType
    , moveAmount :: Int
    } deriving (Show)

move :: CompassDir -> Int -> Coords -> Coords
move North amt (ew, ns) = (ew, ns+amt)
move South amt (ew, ns) = (ew, ns-amt)
move East amt (ew, ns) = (ew+amt, ns)
move West amt (ew, ns) = (ew-amt, ns)

getInput :: IO [Move]
getInput = do
    inLines <- lines <$> readFile "input.txt"
    return $ parseInput inLines
  where
    parseInput =
        fmap parseLine

    parseLine (dirStr:amountStr) =
        let
            dir =
                case dirStr of
                    'N' -> MoveDir North
                    'S' -> MoveDir South
                    'E' -> MoveDir East
                    'W' -> MoveDir West
                    'L' -> Turn Left
                    'R' -> Turn Right
                    'F' -> MoveForward
        in
        Move dir (read amountStr)

problem1 :: IO ()
problem1 = do
    input <- getInput
    print $ solve input
  where
    solve input =
        uncurry (\ew ns -> abs ew + abs ns)
        $ snd
        $ foldl (\(currentDir, ship) (Move moveType moveAmount) ->
            case moveType of
                MoveDir dir -> (currentDir, move dir moveAmount ship)
                Turn t -> (turn t moveAmount currentDir, ship)
                MoveForward -> (currentDir, move currentDir moveAmount ship)
        ) (East, (0,0)) input

    turn :: Turn -> Int -> CompassDir -> CompassDir
    turn t amt currentDir =
        let
            amount =
                case t of
                    Left -> (-1) * (amt `div` 90)
                    Right -> amt `div` 90
        in
        toEnum $ (fromEnum currentDir + amount) `mod` 4

problem2 :: IO ()
problem2 = do
    input <- getInput
    print $ solve input
  where
    solve input =
        uncurry (\ew ns -> abs ew + abs ns)
        $ snd
        $ foldl (\(waypoint, ship) (Move moveType moveAmount) ->
            case moveType of
                MoveDir dir -> (move dir moveAmount waypoint, ship)
                Turn t -> (rotateN moveAmount t waypoint, ship)
                MoveForward -> (waypoint, iterate (moveTowards waypoint) ship !! moveAmount)
        ) ((10,1), (0,0)) input

    moveTowards :: Coords -> Coords -> Coords
    moveTowards to@(ewt, nst) from@(ewf, nsf) =
        (ewt + ewf, nst + nsf)

    rotate90 :: Turn -> Coords -> Coords
    rotate90 t (ew, ns) =
        case t of
            Left -> (-ns, ew)
            Right -> (ns, -ew)

    rotateN :: Int -> Turn -> Coords -> Coords
    rotateN deg t coords =
        iterate (rotate90 t) coords !! (deg `div` 90 `mod` 4)

-- Main --

main :: IO ()
main = do
    [problem] <- getArgs
    case problem of
        "1" -> problem1
        "2" -> problem2
