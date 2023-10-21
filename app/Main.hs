import System.IO
import System.Environment

import Parser
import AbstractSyntaxTree


main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (head args)
    
    let ast = parse contents

    putStrLn (astToStr ast)

