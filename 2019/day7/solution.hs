{- NOTE: Both answers here rely on the solution to Day 5
--       being compiled to an executable and located at
--       ../day5/solution.
-}

import qualified System.Process as Proc
import System.Process (ProcessHandle, CreateProcess(..), StdStream(..))
import System.IO (Handle, hPutStrLn, hFlush, hGetLine)
import Data.List (permutations)
import Control.Monad (foldM, void)
import Data.Maybe (isNothing)
import Control.Concurrent (threadDelay)

-- Debugging
import Debug.Trace

-- Static
programLocation = "../day5/solution"

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

-- PROBLEM 2 (not yet working properly) --

problem2 :: IO ()
problem2 = do
    thrusterOuts <- mapM runThrustersFeedback $ permutations [5..9]
    print $ maximum thrusterOuts

type ProcIOHandle = (Handle, Handle, ProcessHandle)

sendToStdin :: ProcIOHandle -> Int -> IO ()
sendToStdin (inHandle, _, _) val =
    hPutStrLn inHandle (traceShowId $ show val) >> hFlush inHandle

getLineFromStdout :: ProcIOHandle -> IO Int
getLineFromStdout (_, outHandle, _) =
    read <$> hGetLine outHandle

isProcessRunning :: ProcIOHandle -> IO Bool
isProcessRunning (_, _, procHandle) =
    Proc.getProcessExitCode procHandle >>= return . isNothing

runThrustersFeedback :: [Int] -> IO Int
runThrustersFeedback phases = do
    ampPrograms <- mapM (\phase -> do
            (Just inHandle, Just outHandle, _, ph) <-
                Proc.createProcess (Proc.proc programLocation []) {
                    std_in = CreatePipe,
                    std_out = CreatePipe
                }
            hPutStrLn inHandle (traceShowId $ show phase)
            return (inHandle, outHandle, ph)
        ) phases
    runFeedbackLoop 0 (cycle ampPrograms)
  where
    runFeedbackLoop thrustSignal (prog:programs) = do
        void $ sendToStdin prog thrustSignal
        outThrust <- getLineFromStdout prog
        stillRunning <- isProcessRunning prog

        -- debugging
        putStrLn . show $ (thrustSignal, outThrust, stillRunning)

        if stillRunning
            then runFeedbackLoop outThrust programs
            else return outThrust
