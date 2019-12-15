{-# LANGUAGE NamedFieldPuns #-}

import Data.Array.IO
import Control.Monad (void)
import System.IO (hFlush, hPutStr, stdout, stderr)

type Memory = IOUArray Int Int

data Program = Program
    { progRelativeBase :: Int
    , progMemory :: Memory
    }

data Pointer = Ref Int | Rel Int
  deriving (Show)

data Param = Mem Pointer | Val Int
  deriving (Show)

data Op
    = Add Param Param Pointer
    | Mul Param Param Pointer
    | In Pointer
    | Out Param
    | JumpIfTrue Param Param
    | JumpIfFalse Param Param
    | LessThan Param Param Pointer
    | Equals Param Param Pointer
    | AdjustRelativeBase Param
    | Halt
  deriving (Show)

parseOpAt :: Int -> Memory -> IO Op
parseOpAt ix mem = do
    opCode <- readArray mem ix
    let intOp = opCode `mod` 100
        paramModeAt n = opCode `div` (10 ^ (n+1)) `mod` 10

        param :: Int -> IO Param
        param n =
            let p = case paramModeAt n of
                        0 -> Mem . Ref
                        1 -> Val
                        2 -> Mem . Rel
            in do
            val <- readArray mem (ix + n)
            return $ p val

        pointerParam :: Int -> IO Pointer
        pointerParam n =
            let p = case paramModeAt n of
                    0 -> Ref
                    2 -> Rel
                    s -> error (show (s, opCode))
            in do
            val <- readArray mem (ix + n)
            return $ p val

    case intOp of
        1 -> Add <$> (param 1)
                 <*> (param 2)
                 <*> (pointerParam 3)
        2 -> Mul <$> (param 1)
                 <*> (param 2)
                 <*> (pointerParam 3)
        3 -> In  <$> (pointerParam 1)
        4 -> Out <$> (param 1)
        5 -> JumpIfTrue  <$> (param 1)
                         <*> (param 2)
        6 -> JumpIfFalse <$> (param 1)
                         <*> (param 2)
        7 -> LessThan <$> (param 1)
                      <*> (param 2)
                      <*> (pointerParam 3)
        8 -> Equals   <$> (param 1)
                      <*> (param 2)
                      <*> (pointerParam 3)
        9 -> AdjustRelativeBase <$> (param 1)
        99 -> return Halt
        _ -> error $ "Invalid op code: " <> show intOp

getValue :: Param -> Program -> IO Int
getValue (Val n) _ = return n
getValue (Mem (Ref ix)) (Program { progMemory }) = readArray progMemory ix
getValue (Mem (Rel ix)) (Program { progRelativeBase, progMemory }) =
    readArray progMemory (ix + progRelativeBase)

setValue :: Int -> Pointer -> Program -> IO ()
setValue val pointer prog@(Program {progRelativeBase, progMemory }) =
    let ix = case pointer of
            Ref addr -> addr
            Rel offset -> progRelativeBase + offset
    in do
    writeArray progMemory ix val

eval :: Int -> Program -> IO Program
eval ix prog@(Program { progRelativeBase, progMemory }) = do
    opt <- parseOpAt ix progMemory
    case opt of
        Add p1 p2 dest -> do
            newVal <- (+) <$> getValue p1 prog <*> getValue p2 prog
            setValue newVal dest prog
            eval (ix + 4) prog

        Mul p1 p2 dest -> do
            newVal <- (*) <$> getValue p1 prog <*> getValue p2 prog
            setValue newVal dest prog
            eval (ix + 4) prog

        In dest -> do
            hPutStr stderr "Input: "
            hFlush stderr
            inNum <- read <$> getLine
            setValue inNum dest prog
            eval (ix + 2) prog

        Out p1 -> do
            getValue p1 prog >>= putStrLn . show
            hFlush stdout
            eval (ix + 2) prog

        JumpIfTrue p1 p2 ->
            jumpIf (/=0) p1 p2

        JumpIfFalse p1 p2 ->
            jumpIf (==0) p1 p2

        LessThan p1 p2 dest ->
            compareParams (<) p1 p2 dest

        Equals p1 p2 dest ->
            compareParams (==) p1 p2 dest

        AdjustRelativeBase p1 -> do
            offset <- getValue p1 prog
            eval
                (ix + 2)
                (prog { progRelativeBase = progRelativeBase + offset })

        Halt ->
            return prog
  where
    jumpIf pred p1 p2 = do
        p1Val <- getValue p1 prog
        p2Val <- getValue p2 prog
        if pred p1Val
            then eval p2Val prog
            else eval (ix + 3) prog

    compareParams pred p1 p2 dest = do
        p1Val <- getValue p1 prog
        p2Val <- getValue p2 prog
        if pred p1Val p2Val
            then do
                setValue 1 dest prog
                eval (ix + 4) prog
            else do
                setValue 0 dest prog
                eval (ix + 4) prog

getInput :: IO Program
getInput = do
    input <- readFile "input.txt"
    let inputAsList = read ("[" <> input <> "]") <> take 5000 [0,0..]
    arr <- newListArray (0, length inputAsList - 1) inputAsList
    return $ Program 0 arr

main :: IO ()
main = do
    input <- getInput
    void $ eval 0 input
