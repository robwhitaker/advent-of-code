{-# LANGUAGE ViewPatterns #-}

module Day03 (problem1, problem2) where

import Data.Char (isUpper, ord)
import Data.Functor ((<&>))
import Data.List.Split (chunksOf)
import Data.Set qualified as S

score :: Char -> Integer
score ch
  | isUpper ch = toInteger (ord ch) - 38
  | otherwise = toInteger (ord ch) - 96

problem1 :: FilePath -> IO Integer
problem1 file = do
  input <- readFile file <&> fmap (\items -> splitAt (length items `div` 2) items) . lines
  pure $ sum $ fmap scoreRucksack input
  where
    scoreRucksack :: ([Char], [Char]) -> Integer
    scoreRucksack (S.fromList -> compartment1, S.fromList -> compartment2) =
      score $ S.elemAt 0 $ S.intersection compartment1 compartment2

problem2 :: FilePath -> IO Integer
problem2 file = do
  input <- chunksOf 3 . lines <$> readFile file
  pure $ sum $ fmap scoreRucksacks input
  where
    scoreRucksacks :: [[Char]] -> Integer
    scoreRucksacks (fmap S.fromList -> rucksacks) =
      score $ S.elemAt 0 $ foldr1 S.intersection rucksacks
