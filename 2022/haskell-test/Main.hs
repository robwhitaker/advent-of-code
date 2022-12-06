{-# LANGUAGE DerivingStrategies #-}

module Main (main) where

import Control.Monad (forM_)
import Data.Char (toLower)
import Day01 qualified
import Day02 qualified
import Day03 qualified
import Day04 qualified
import Day05 qualified
import Day06 qualified
import Paths_AoC2022 (getDataFileName)
import Test.Hspec

data Output
  = String String
  | Integer Integer
  | Unknown
  deriving stock (Show, Eq)

problems :: [(String, Int, FilePath -> IO Output, Output, Output, [(String, Output)])]
problems =
  [ ("Day01", 1, fmap Integer . Day01.problem1, Integer 24000, Integer 67450, []),
    ("Day01", 2, fmap Integer . Day01.problem2, Integer 45000, Integer 199357, []),
    ("Day02", 1, fmap Integer . Day02.problem1, Integer 15, Integer 11150, []),
    ("Day02", 2, fmap Integer . Day02.problem2, Integer 12, Integer 8295, []),
    ("Day03", 1, fmap Integer . Day03.problem1, Integer 157, Integer 7863, []),
    ("Day03", 2, fmap Integer . Day03.problem2, Integer 70, Integer 2488, []),
    ("Day04", 1, fmap Integer . Day04.problem1, Integer 2, Integer 560, []),
    ("Day04", 2, fmap Integer . Day04.problem2, Integer 4, Integer 839, []),
    ("Day05", 1, fmap String . Day05.problem1, String "CMZ", String "CWMTGHBDW", []),
    ("Day05", 2, fmap String . Day05.problem2, String "MCD", String "SSCGWJCRB", []),
    ( "Day06",
      1,
      fmap Integer . Day06.problem1,
      Integer 7,
      Integer 1647,
      [ ("input_sample_2.txt", Integer 5),
        ("input_sample_3.txt", Integer 6),
        ("input_sample_4.txt", Integer 10),
        ("input_sample_5.txt", Integer 11)
      ]
    ),
    ( "Day06",
      2,
      fmap Integer . Day06.problem2,
      Integer 19,
      Integer 2447,
      [ ("input_sample_2.txt", Integer 23),
        ("input_sample_3.txt", Integer 23),
        ("input_sample_4.txt", Integer 29),
        ("input_sample_5.txt", Integer 26)
      ]
    )
  ]

main :: IO ()
main = hspec $ do
  forM_ problems $ \(day, part, solver, sampleAnswer, realAnswer, extraSamples) -> do
    sampleInputFile <- runIO $ getDataFileName ("problems/" <> fmap toLower day <> "/input_sample.txt")
    realInputFile <- runIO $ getDataFileName ("problems/" <> fmap toLower day <> "/input.txt")

    describe (day <> ".Problem" <> show part) $ do
      it "has the right answer for the sample input" $ do
        solver sampleInputFile `shouldReturn` sampleAnswer

      forM_ extraSamples $ \(file, answer) -> do
        extraSampleFile <- runIO $ getDataFileName ("problems/" <> fmap toLower day <> "/" <> file)
        it ("has the right answer for the extra sample input: " <> file) $ do
          solver extraSampleFile `shouldReturn` answer

      it "has the right answer for the real input" $ do
        solver realInputFile `shouldReturn` realAnswer
