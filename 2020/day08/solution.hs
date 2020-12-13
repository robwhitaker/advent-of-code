import Prelude hiding (or)

type Visited = Bool

data Program = Program
    { acc :: Int
    , instructions :: SelectionList (Instruction, Visited)
    }

data Op = Acc | Jmp | Nop deriving (Show)

data Instruction = Instruction
    { op :: Op
    , arg :: Int
    } deriving (Show)

data SelectionList a = SL
    { before :: [a]
    , selected :: a
    , after :: [a]
    } deriving (Show)

instance Functor SelectionList where
    fmap f (SL before selected after) =
        SL (fmap f before) (f selected) (fmap f after)

selectionList :: a -> [a] -> SelectionList a
selectionList selected rest =
    SL [] selected rest

next :: SelectionList a -> SelectionList a
next sl@(SL before selected after) =
    case after of
        (x:xs) ->
            SL (selected:before) x xs
        [] ->
            sl

nextN :: Int -> SelectionList a -> SelectionList a
nextN n sl =
    (iterate next sl) !! n

prev :: SelectionList a -> SelectionList a
prev sl@(SL before selected after) =
    case before of
        (x:xs) ->
            SL xs x (selected:after)
        [] ->
            sl

prevN :: Int -> SelectionList a -> SelectionList a
prevN n sl =
    (iterate prev sl) !! n

modifyCurrent :: (a -> a) -> SelectionList a -> SelectionList a
modifyCurrent f (SL before selected after) =
    SL before (f selected) after

atEnd :: SelectionList a -> Bool
atEnd (SL _ _ []) = True
atEnd _           = False

getInput :: IO Program
getInput = do
    inLines <- lines <$> readFile "input.txt"
    return $ parseInput inLines
  where
    parseInput inLines =
        let (x:xs) = zip (fmap parseLine inLines) (repeat False)
        in
        Program 0 (selectionList x xs)

    parseLine ln =
        let [op, arg] = words ln
        in
        Instruction (parseOp op) (read $ filter (/='+') arg)

    parseOp op =
        case op of
            "acc" -> Acc
            "jmp" -> Jmp
            "nop" -> Nop
            _     -> error $ "Error: no such op: \"" <> op <> "\""

markVisited :: SelectionList (Instruction, Visited) -> SelectionList (Instruction, Visited)
markVisited =
    modifyCurrent $ \(inst, _) -> (inst, True)

isRepeatInstruction :: Program -> Bool
isRepeatInstruction =
    snd . selected . instructions

currentOp :: Program -> Op
currentOp =
    op . fst . selected . instructions

runProgramStep :: Program -> Either Program Int -- Left (program still running), Right (returned acc code)
runProgramStep (Program acc instructions) =
    case fst (selected instructions) of
        (Instruction Acc n) ->
            if atEnd instructions
            then Right (acc + n)
            else Left $ Program (acc + n)
                                (next $ markVisited instructions)
        (Instruction Jmp n) ->
            let continue = if n < 0 then prevN else nextN
            in if n > length (after instructions)
               then Right acc
               else Left $ Program acc (continue (abs n) $ markVisited instructions)
        (Instruction Nop _) ->
            if atEnd instructions
            then Right acc
            else Left $ Program acc (next $ markVisited instructions)

runProgram :: Program -> Either Int Int -- Left acc on fail, Right acc on termination
runProgram program =
    if isRepeatInstruction program
    then Left (acc program)
    else case runProgramStep program of
             Left steppedProgram ->
                 runProgram steppedProgram
             Right returnedAcc ->
                 Right returnedAcc

problem1 :: IO ()
problem1 = do
    input <- getInput
    print $ solve input
  where
    solve input =
        case runProgram input of
            Left errAcc ->
                errAcc
            Right _ ->
                error "Program shouldn't complete."


problem2 :: IO ()
problem2 = do
    input <- getInput
    print $ solve input
  where
    solve program =
        case currentOp program of
            Acc -> step program
            _ -> either id id $ runProgram (swapCurrentOp program)
                           `or` Right (step program)
      where
        step = either solve id . runProgramStep

    swapCurrentOp :: Program -> Program
    swapCurrentOp program =
        case currentOp program of
            Acc -> program
            Jmp -> program { instructions = modifyCurrent (changeOpTo Nop) (instructions program) }
            Nop -> program { instructions = modifyCurrent (changeOpTo Jmp) (instructions program) }
      where
        changeOpTo op (inst, visited) =
            (Instruction op (arg inst), visited)

    or :: Either a b -> Either a b -> Either a b
    or e1 e2 =
        case e1 of
            Left _ -> e2
            Right _ -> e1

-- Utils --

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn needle inList =
    go [] [] inList
  where
    go acc curList (x:xs) =
        if x == needle
        then go (reverse curList:acc) [] xs
        else go acc (x:curList) xs
    go acc curList [] =
        reverse $ reverse curList:acc
