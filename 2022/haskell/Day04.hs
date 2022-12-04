{-# LANGUAGE ViewPatterns #-}

module Day04 (problem1, problem2) where

import Data.Functor ((<&>))
import Data.List.Split (splitOn)
import Data.Set (Set)
import Data.Set qualified as S

type Input = (Set Integer, Set Integer)

readInput :: FilePath -> IO [Input]
readInput file = do
  fmap parseLine . lines <$> readFile file
  where
    parseLine :: String -> Input
    parseLine str =
      case splitOn "," str of
        [fmap read . splitOn "-" -> [r1s, r1e], fmap read . splitOn "-" -> [r2s, r2e]] ->
          (S.fromList [r1s .. r1e], S.fromList [r2s .. r2e])
        _ -> error "bad input"

problem1 :: FilePath -> IO Integer
problem1 file = do
  toInteger . length . filter oneIsSubset <$> readInput file
  where
    oneIsSubset :: (Set Integer, Set Integer) -> Bool
    oneIsSubset (s1, s2)
      | S.size s1 >= S.size s2 = s2 `S.isSubsetOf` s1
      | otherwise = s1 `S.isSubsetOf` s2

problem2 :: FilePath -> IO Integer
problem2 file = do
  readInput file
    <&> toInteger
      . length
      . filter (\(s1, s2) -> not $ S.null $ s1 `S.intersection` s2)
