{-# LANGUAGE DerivingStrategies #-}

module Day06 (problem1, problem2) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M

findMarker :: Int -> String -> Integer
findMarker len input =
  toInteger $ go 0 input (M.fromListWith (+) (zip (take len input) (repeat 1))) (drop len input)
  where
    go :: Int -> String -> Map Char Int -> String -> Int
    go ix (oldest : history) window (next : rest)
      | M.size window == len = ix + len
      | otherwise =
          go (ix + 1) history (M.insertWith (+) next 1 . decrementOrRemove oldest $ window) rest
    go _ _ _ _ = error "bad input"

    decrementOrRemove :: Char -> Map Char Int -> Map Char Int
    decrementOrRemove =
      M.update
        ( \count ->
            if count - 1 <= 0 then Nothing else Just (count - 1)
        )

problem1 :: FilePath -> IO Integer
problem1 file = do
  findMarker 4 <$> readFile file

problem2 :: FilePath -> IO Integer
problem2 file = do
  findMarker 14 <$> readFile file
