module Main (main) where

import Control.Monad (forM_)
import Data.Char (toLower)
import Day01 qualified
import Paths_AoC2022 (getDataFileName)
import Test.Hspec

problems :: [(String, Int, FilePath -> IO Integer, Integer, Integer)]
problems =
  [ ("Day01", 1, Day01.problem1, 24000, 67450),
    ("Day01", 2, Day01.problem2, 45000, 199357)
  ]

main :: IO ()
main = hspec $ do
  forM_ problems $ \(day, part, solver, sampleAnswer, realAnswer) -> do
    sampleInputFile <- runIO $ getDataFileName ("problems/" <> fmap toLower day <> "/input_sample.txt")
    realInputFile <- runIO $ getDataFileName ("problems/" <> fmap toLower day <> "/input.txt")

    describe (day <> ".Problem" <> show part) $ do
      it "has the right answer for the sample input" $ do
        solver sampleInputFile `shouldReturn` sampleAnswer

      it "has the right answer for the real input" $ do
        solver realInputFile `shouldReturn` realAnswer
