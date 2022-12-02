module Day01 (problem1, problem2) where

import Data.List (sort)
import Data.List.Split (splitOn)

type Snack = Integer

readInput :: FilePath -> IO [[Snack]]
readInput file = do
  fmap (fmap read) . splitOn [""] . lines <$> readFile file

problem1 :: FilePath -> IO Integer
problem1 file = do
  maximum . fmap sum <$> readInput file

problem2 :: FilePath -> IO Integer
problem2 file = do
  sum . take 3 . reverse . sort . fmap sum <$> readInput file
