import Data.List (splitAt)
import qualified Data.Array as Array
import Data.Array (Array, (!))
import Data.Char (isNumber)
import Control.Monad (forM_)

type Dimensions = (Int, Int)
type Pixels = Array Int Int
data Image = Image Dimensions Pixels

getInput :: IO Pixels
getInput = do
    input <- fmap (read . (:[])) . filter isNumber <$> readFile "input.txt"
    return $ Array.listArray (0, length input - 1) input

-- PROBLEM 1 --

problem1 :: IO ()
problem1 = do
    pixels <- getInput
    let img = Image (25, 6) pixels
        fewestZeroLayer = findFewestZeroDigits img
        ones = length $ filter (==1) fewestZeroLayer
        twos = length $ filter (==2) fewestZeroLayer
    print $ ones * twos

findFewestZeroDigits :: Image -> [Int]
findFewestZeroDigits (Image (width, height) pixels) =
    recurse (maxBound, []) (Array.elems pixels)
  where
    layerSize = width * height
    recurse (_, xs) [] = xs
    recurse acc px =
        let (thisLayer, rest) = splitAt layerSize px
            zeroes = length $ filter (==0) thisLayer
        in
        recurse (min acc (zeroes, thisLayer)) rest

-- PROBLEM 2 --

problem2 :: IO ()
problem2 = do
    pixels <- getInput
    let (width, height) = (25, 6)
    let img = Image (width, height) pixels
    forM_ [0..height-1] $ \y -> do
        forM_ [0..width-1] $ \x -> do
            let color = getColorAt (x, y) img
                black = "\ESC[40m \ESC[0m"
                white = "\ESC[40;3m \ESC[0m"
            putStr $ if color == 0 then black else white
        putStrLn ""

getColorAt :: (Int, Int) -> Image -> Int
getColorAt (x, y) (Image (width, height) pixels) =
    case dropWhile (==2) (fmap (pixels !) layerIndices) of
        [] -> 2
        (x:_) -> x
  where
    offset = x + y*width
    layerSize = width * height
    layerIndices = iterate (+layerSize) offset
