module Interpreter
( interpret
) where

import AbstractSyntaxTree
import System.IO

data Runtime = Runtime {
    tape :: [Int],
    idx :: Int
}

newRuntime = Runtime tape 0
    where
        size = 50000
        tape = take size $ repeat 0

currentCell runtime = (tape runtime) !! (idx runtime)

mutPtr :: Runtime -> (Int -> Int) -> Runtime
mutPtr runtime mutator = Runtime (tape runtime) (mutator $ idx runtime)

incPtr :: Runtime -> Runtime
incPtr runtime = mutPtr runtime succ

decPtr :: Runtime -> Runtime
decPtr runtime = mutPtr runtime pred

mutVal :: Runtime -> (Int -> Int) -> Runtime
mutVal runtime mutator = Runtime newTape (idx runtime)
    where
        begin = take (idx runtime) (tape runtime)
        (item : rest) = drop (idx runtime) (tape runtime)
        newItem = mutator item
        end = newItem : rest
        newTape = begin ++ end

incVal :: Runtime -> Runtime
incVal runtime = mutVal runtime incrementor
    where
        incrementor 255 = 0
        incrementor num = succ num

decVal :: Runtime -> Runtime
decVal runtime = mutVal runtime decrementor
    where
        decrementor 0 = 255
        decrementor num = pred num

readByte :: Runtime -> IO Runtime
readByte runtime = do

    let createSetter num _   = num
        newRuntime num = mutVal runtime (createSetter num)

    eof <- isEOF

    if eof then
        return (newRuntime 0)
    else do
        char <- getChar
        return (newRuntime $ fromEnum char)


writeByte' :: Runtime -> IO ()
writeByte' runtime = putChar char
    where
        char = toEnum $ currentCell runtime

writeByte :: Runtime -> IO Runtime
writeByte runtime = do
    writeByte' runtime
    return runtime

interpretLoop :: Runtime -> Node -> IO Runtime
interpretLoop runtime loop
    | currentCell runtime == 0 = return runtime
    | otherwise = do
        let nodes = getNodes loop
        newRuntime <- interpretNodes runtime nodes
        interpretLoop newRuntime loop

interpretNode :: Runtime -> Node -> IO Runtime
interpretNode runtime (Loop nodes)   = interpretLoop runtime (Loop nodes)
interpretNode runtime (Operator '+') = return (incVal runtime)
interpretNode runtime (Operator '-') = return (decVal runtime)
interpretNode runtime (Operator '>') = return (incPtr runtime)
interpretNode runtime (Operator '<') = return (decPtr runtime)
interpretNode runtime (Operator ',') = readByte runtime
interpretNode runtime (Operator '.') = writeByte runtime

interpretNodes :: Runtime -> [Node] -> IO Runtime
interpretNodes runtime [] = return runtime
interpretNodes runtime (node : nodes) = do
    newRuntime <- interpretNode runtime node
    interpretNodes newRuntime nodes

interpret :: Program -> IO ()
interpret program = do
    let runtime = newRuntime
    interpretNodes runtime program
    return ()
