{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Day02 (problem1, problem2) where

import Control.Category ((>>>))
import Data.Functor ((<&>))

data Move = Rock | Paper | Scissors

data Outcome = Win | Lose | Draw

data Problem a where
  Problem1 :: Problem Move
  Problem2 :: Problem Outcome

char2Move :: Char -> Move
char2Move = \case
  'A' -> Rock
  'B' -> Paper
  'C' -> Scissors
  'X' -> Rock
  'Y' -> Paper
  'Z' -> Scissors
  _ -> error "Invalid char"

char2Outcome :: Char -> Outcome
char2Outcome = \case
  'X' -> Lose
  'Y' -> Draw
  'Z' -> Win
  _ -> error "Invalid char"

move2Score :: Move -> Integer
move2Score = \case
  Rock -> 1
  Paper -> 2
  Scissors -> 3

outcome2Score :: Outcome -> Integer
outcome2Score = \case
  Win -> 6
  Draw -> 3
  Lose -> 0

readInput :: FilePath -> Problem a -> IO [(Move, a)]
readInput file problem = do
  readFile file
    <&> ( lines
            >>> fmap words
            >>> fmap
              ( \case
                  [[o], [p]] ->
                    ( char2Move o,
                      case problem of
                        Problem1 -> char2Move p
                        Problem2 -> char2Outcome p
                    )
                  _ -> error "bad input"
              )
        )

problem1 :: FilePath -> IO Integer
problem1 file = do
  readInput file Problem1 <&> sum . fmap score
  where
    play :: (Move, Move) -> Outcome
    play (Rock, Paper) = Win
    play (Scissors, Rock) = Win
    play (Paper, Scissors) = Win
    play (Paper, Rock) = Lose
    play (Rock, Scissors) = Lose
    play (Scissors, Paper) = Lose
    play _ = Draw

    score :: (Move, Move) -> Integer
    score moves@(_, you) =
      move2Score you + outcome2Score (play moves)

problem2 :: FilePath -> IO Integer
problem2 file = do
  readInput file Problem2 <&> sum . fmap score
  where
    play :: (Move, Outcome) -> Move
    play (Rock, Win) = Paper
    play (Scissors, Win) = Rock
    play (Paper, Win) = Scissors
    play (Paper, Lose) = Rock
    play (Rock, Lose) = Scissors
    play (Scissors, Lose) = Paper
    play (move, Draw) = move

    score :: (Move, Outcome) -> Integer
    score theRound@(_, outcome) =
      move2Score (play theRound) + outcome2Score outcome
