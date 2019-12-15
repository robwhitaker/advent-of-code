{- Requires Day 9 to be compiled to ../day9/solution -}

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (sortOn)
import System.Process (createProcess, getProcessExitCode, shell, CreateProcess(..), StdStream(..))
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import System.IO (hPutStrLn, hFlush, hGetLine)
import Data.Maybe (fromMaybe)

data Color = Black | White
  deriving (Show, Enum, Eq)

data Move = TurnLeft | TurnRight
  deriving (Show, Enum, Eq)

data Dir = U | R | D | L
  deriving (Show, Eq, Enum)

type Coord = (Int, Int)

type Hull = Map Coord Color

enum :: Enum a => String -> a
enum = toEnum . read

unEnum :: Enum a => a -> String
unEnum = show . fromEnum

move :: Coord -> Dir -> Move -> (Coord, Dir)
move (x, y) facing move =
    case toEnum ((fromEnum facing + turnBy) `mod` 4) of
        U -> ((x, y-1), U)
        R -> ((x+1, y), R)
        D -> ((x, y+1), D)
        L -> ((x-1, y), L)
  where
    turnBy =
        case move of
            TurnLeft -> 3
            TurnRight -> 1

runRobot :: Color -> IO Hull
runRobot startingColor = do
    (Just robotIn, Just robotOut, _, ph) <- createProcess (shell "../day9/solution") {
        std_in = CreatePipe,
        std_out = CreatePipe
    }
    goUntilDone (robotIn, robotOut, ph) (Map.empty) (0, 0) U (Just startingColor)
  where
    goUntilDone handles@(robotIn, robotOut, ph) hull loc facing mbStartColor = do
        let defColor = fromMaybe Black mbStartColor
            panelColor = Map.findWithDefault defColor loc hull
        hPutStrLn robotIn (unEnum panelColor)
        hFlush robotIn
        paintColor <- enum <$> hGetLine robotOut
        turnDir <- enum <$> hGetLine robotOut
        let (newLoc, newFacing) = move loc facing turnDir
            newHull = Map.insert loc paintColor hull
        threadDelay 100
        mbExitCode <- getProcessExitCode ph
        case mbExitCode of
            Nothing ->
                goUntilDone handles newHull newLoc newFacing Nothing

            Just _ ->
                return hull

problem1 :: IO ()
problem1 = runRobot Black >>= print . Map.size

problem2 :: IO ()
problem2 = do
    hull <- runRobot White
    putStrLn ""
    let coords = Map.keys hull
        xs = map fst coords
        ys = map snd coords
        maxX = maximum xs
        minX = minimum xs
        maxY = maximum ys
        minY = minimum ys
        black = "\ESC[40m \ESC[0m"
        white = "\ESC[40;3m \ESC[0m"
    forM_ [minY..maxY] $ \y -> do
        forM_ [minX..maxX] $ \x -> do
            putStr $ case Map.lookup (x, y) hull of
                Nothing -> " "
                Just Black -> black
                Just White -> white
        putStrLn ""
