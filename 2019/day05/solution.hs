import Data.Array
import Control.Monad (void)
import System.IO (hFlush, stdout)

type Program = Array Int Int

data Param = Ref Int | Val Int

type Ref = Int
data Op
    = Add Param Param Ref
    | Mul Param Param Ref
    | In Ref
    | Out Ref
    | JumpIfTrue Param Param
    | JumpIfFalse Param Param
    | LessThan Param Param Ref
    | Equals Param Param Ref
    | Halt

parseOpAt :: Int -> Program -> Op
parseOpAt ix arr =
    case intOp of
        1 -> Add (param 1) (param 2) (refParam 3)
        2 -> Mul (param 1) (param 2) (refParam 3)
        3 -> In (refParam 1)
        4 -> Out (refParam 1)
        5 -> JumpIfTrue (param 1) (param 2)
        6 -> JumpIfFalse (param 1) (param 2)
        7 -> LessThan (param 1) (param 2) (refParam 3)
        8 -> Equals (param 1) (param 2) (refParam 3)
        99 -> Halt
        _ -> error $ "Invalid op code: " <> show intOp
  where
    opCode = arr ! ix
    intOp = opCode `mod` 100
    param n =
        let paramModeNum = opCode `div` (10 ^ (n+1)) `mod` 10
            p = if paramModeNum == 0 then Ref else Val
        in
        p (arr ! (ix + n))
    refParam n =
        arr ! (ix + n)

getValue :: Param -> Program -> Int
getValue (Val n) _ = n
getValue (Ref ix) arr = arr ! ix

setValue :: Int -> Ref -> Program -> Program
setValue val ix arr =
    arr // [(ix, val)]

eval :: Int -> Program -> IO Program
eval ix arr =
    case parseOpAt ix arr of
        Add p1 p2 dest ->
            eval (ix + 4) (setValue (getValue p1 arr + getValue p2 arr) dest arr)

        Mul p1 p2 dest ->
            eval (ix + 4) (setValue (getValue p1 arr * getValue p2 arr) dest arr)

        In dest -> do
            inNum <- read <$> getLine
            eval (ix + 2) (setValue inNum dest arr)

        Out ref -> do
            putStrLn . show $ (arr ! ref)
            hFlush stdout
            eval (ix + 2) arr

        JumpIfTrue p1 p2 ->
            jumpIf (/=0) p1 p2

        JumpIfFalse p1 p2 ->
            jumpIf (==0) p1 p2

        LessThan p1 p2 dest ->
            compareParams (<) p1 p2 dest

        Equals p1 p2 dest ->
            compareParams (==) p1 p2 dest

        Halt ->
            return arr
  where
    jumpIf pred p1 p2 =
        if pred (getValue p1 arr)
            then eval (getValue p2 arr) arr
            else eval (ix + 3) arr

    compareParams pred p1 p2 dest =
        if pred (getValue p1 arr) (getValue p2 arr)
            then eval (ix + 4) (setValue 1 dest arr)
            else eval (ix + 4) (setValue 0 dest arr)

getInput :: IO Program
getInput = do
    input <- readFile "input.txt"
    let inputAsList = read ("[" <> input <> "]")
    return $ listArray (0, length inputAsList - 1) inputAsList

-- Answers both problem 1 & 2 since the answers only vary by runtime input
-- Input for problem 1: 1
-- Input for problem 2: 5
main :: IO ()
main = do
    input <- getInput
    void $ eval 0 input
