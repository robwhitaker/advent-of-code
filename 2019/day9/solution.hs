{-# LANGUAGE NamedFieldPuns #-}

import Data.Array
import Control.Monad (void)
import System.IO (hFlush, hPutStr, stdout, stderr)

import Debug.Trace

type Memory = Array Int Int

data Program = Program
    { progRelativeBase :: Int
    , progMemory :: Memory
    } deriving (Show)

data Pointer = Ref Int | Rel Int
  deriving (Show)

data Param = Mem Pointer | Val Int
  deriving (Show)

data Op
    = Add Param Param Pointer
    | Mul Param Param Pointer
    | In Pointer
    | Out Pointer
    | JumpIfTrue Param Param
    | JumpIfFalse Param Param
    | LessThan Param Param Pointer
    | Equals Param Param Pointer
    | AdjustRelativeBase Param
    | Halt
  deriving (Show)

parseOpAt :: Int -> Memory -> Op
parseOpAt ix mem =
    case intOp of
        1 -> Add (param 1) (param 2) (pointerParam 3)
        2 -> Mul (param 1) (param 2) (pointerParam 3)
        3 -> In (pointerParam 1)
        4 -> Out (pointerParam 1)
        5 -> JumpIfTrue (param 1) (param 2)
        6 -> JumpIfFalse (param 1) (param 2)
        7 -> LessThan (param 1) (param 2) (pointerParam 3)
        8 -> Equals (param 1) (param 2) (pointerParam 3)
        9 -> AdjustRelativeBase (param 1)
        99 -> Halt
        _ -> error $ "Invalid op code: " <> show intOp
  where
    opCode = mem ! ix
    intOp = opCode `mod` 100
    paramModeAt n = opCode `div` (10 ^ (n+1)) `mod` 10
    param :: Int -> Param
    param n =
        let p = case paramModeAt n of
                    0 -> Mem . Ref
                    1 -> Val
                    2 -> Mem . Rel
        in
        p (mem ! (ix + n))
    pointerParam :: Int -> Pointer
    pointerParam n =
        let p = case paramModeAt n of
                0 -> Ref
                2 -> Rel
        in
        p (mem ! (ix + n))

getValue :: Param -> Program -> Int
getValue (Val n) _ = n
getValue (Mem (Ref ix)) (Program { progMemory }) = progMemory ! ix
getValue (Mem (Rel ix)) (Program { progRelativeBase, progMemory }) =
    progMemory ! (ix + progRelativeBase)

setValue :: Int -> Pointer -> Program -> Program
setValue val pointer prog@(Program {progRelativeBase, progMemory }) =
    let ix = case pointer of
            Ref addr -> addr
            Rel offset -> progRelativeBase + offset
    in
    prog { progMemory = progMemory // [(ix, val)] }

eval :: Int -> Program -> IO Program
eval ix prog@(Program { progRelativeBase, progMemory }) =
    case parseOpAt ix progMemory of
        Add p1 p2 dest ->
            eval (ix + 4) (setValue (getValue p1 prog + getValue p2 prog) dest prog)

        Mul p1 p2 dest ->
            eval (ix + 4) (setValue (getValue p1 prog * getValue p2 prog) dest prog)

        In dest -> do
            hPutStr stderr "Input: "
            hFlush stderr
            inNum <- read <$> getLine
            eval (ix + 2) (setValue inNum dest prog)

        Out pointer -> do
            putStrLn . show $ getValue (Mem pointer) prog
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

        AdjustRelativeBase p1 ->
            eval
                (ix + 2)
                (prog { progRelativeBase = progRelativeBase + getValue p1 prog })

        Halt ->
            return prog
  where
    jumpIf pred p1 p2 =
        if pred (getValue p1 prog)
            then eval (getValue p2 prog) prog
            else eval (ix + 3) prog

    compareParams pred p1 p2 dest =
        if pred (getValue p1 prog) (getValue p2 prog)
            then eval (ix + 4) (setValue 1 dest prog)
            else eval (ix + 4) (setValue 0 dest prog)

getInput :: IO Program
getInput = do
    input <- readFile "input.txt"
    let inputAsList = read ("[" <> input <> "]") <> take 5000 [0,0..]
    return $ Program 0 (listArray (0, length inputAsList - 1) inputAsList)

-- Answers both problem 1 & 2 since the answers only vary by runtime input
-- Input for problem 1: 1
-- Input for problem 2: 5
main :: IO ()
main = do
    input <- getInput
    void $ eval 0 input
