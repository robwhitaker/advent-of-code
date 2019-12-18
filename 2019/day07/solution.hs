{- NOTE: Both answers here rely on the solution to Day 5
--       being compiled to an executable and located at
--       ../day05/solution.
-}

import qualified System.Process as Proc
import System.Process (CreateProcess(..), StdStream(..))
import System.IO (hPutStrLn, hFlush, hGetLine)
import System.Exit (exitSuccess)
import Data.List (permutations)
import Control.Monad (forever, foldM, void)
import Data.Maybe (isNothing)
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, forkIO, threadDelay)

-- Static
programLocation = "../day05/solution"

-- PROBLEM 1 --

problem1 :: IO ()
problem1 = do
    thrusterOuts <- mapM runThrusters $ permutations [0..4]
    print $ maximum thrusterOuts

runThrusters :: [Int] -> IO Int
runThrusters =
    foldM (\input phase ->
        read <$> Proc.readProcess programLocation [] (show phase <> "\n" <> show input)
    ) 0

-- PROBLEM 2 --

problem2 :: IO ()
problem2 = do
    thrusterOuts <- mapM runThrustersFeedback $ permutations [5..9]
    print $ maximum thrusterOuts

runThrusterThread :: (Int, Maybe (MVar Int), (MVar Int, MVar Int)) -> IO ()
runThrusterThread (phase, mbMResult, (mInput, mOutput)) =
    void $ forkIO $ do
        (Just inHandle, Just outHandle, _, ph) <-
            Proc.createProcess (Proc.proc programLocation []) {
                std_in = CreatePipe,
                std_out = CreatePipe
            }
        hPutStrLn inHandle (show phase)
        forever $ do
            input <- takeMVar mInput
            hPutStrLn inHandle (show input) >> hFlush inHandle
            threadDelay 250
            output <- read <$> hGetLine outHandle
            threadDelay 250
            stillRunning <- Proc.getProcessExitCode ph >>= return . isNothing
            case (stillRunning, mbMResult) of
                (False, (Just mResult)) ->
                    putMVar mResult output >> exitSuccess

                _ ->
                    putMVar mOutput output

runThrustersFeedback :: [Int] -> IO Int
runThrustersFeedback phases = do
    mResult <- newEmptyMVar
    mvars <- mapM (const newEmptyMVar) phases
    let threadInOuts = zip mvars (tail $ cycle mvars)
        lastPhase = last phases
        mbReturnList = map (\phase ->
                if phase == lastPhase then Just mResult else Nothing
            ) phases
    mapM_ runThrusterThread $ zip3 phases mbReturnList threadInOuts
    let (mFirstInput, _) = head threadInOuts
    putMVar mFirstInput 0
    takeMVar mResult
