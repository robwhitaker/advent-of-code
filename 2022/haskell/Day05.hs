{-# LANGUAGE DerivingStrategies #-}

module Day05 (problem1, problem2) where

import Control.Arrow ((>>>))
import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Function ((&))
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (mapMaybe)
import Data.Vector.Mutable qualified as VM
import Text.Read (readMaybe)

data Input = Input
  { stacks :: [[Char]],
    moves :: [(Int, Int, Int)]
  }
  deriving stock (Show)

readInput :: FilePath -> IO Input
readInput file = do
  inputLines <- lines <$> readFile file
  let stacks =
        inputLines
          & ( takeWhile (not . null)
                >>> reverse . drop 1 . reverse
                >>> fmap (fmap (!! 1) . chunksOf 4)
                >>> transpose
                >>> fmap (dropWhile (== ' '))
            )
      moves =
        inputLines
          & ( dropWhile (not . null)
                >>> drop 1
                >>> fmap parseMoves
            )
  pure $ Input stacks moves
  where
    parseMoves :: String -> (Int, Int, Int)
    parseMoves str =
      case mapMaybe readMaybe (words str) of
        [n, from, to] -> (n, from, to)
        _ -> error "bad input"

problem1 :: FilePath -> IO String
problem1 file = do
  input <- readInput file
  let postMovement =
        runST $ do
          stackVec <- VM.new (length (stacks input))
          mapM_ (uncurry (VM.write stackVec)) (zip [0 ..] (stacks input))
          forM_ (moves input) $ \(amt, from, to) ->
            forM_ [1 .. amt] $ \_ -> do
              fromTop <- head <$> VM.read stackVec (from - 1)
              VM.modify stackVec (drop 1) (from - 1)
              VM.modify stackVec (fromTop :) (to - 1)
          VM.foldr' (:) [] stackVec
  pure $ filter (/= ' ') $ fmap head postMovement

problem2 :: FilePath -> IO String
problem2 file = do
  input <- readInput file
  let postMovement =
        runST $ do
          stackVec <- VM.new (length (stacks input))
          mapM_ (uncurry (VM.write stackVec)) (zip [0 ..] (stacks input))
          forM_ (moves input) $ \(amt, from, to) -> do
            fromTop <- take amt <$> VM.read stackVec (from - 1)
            VM.modify stackVec (drop amt) (from - 1)
            VM.modify stackVec (fromTop <>) (to - 1)
          VM.foldr' (:) [] stackVec
  pure $ filter (/= ' ') $ fmap head postMovement
