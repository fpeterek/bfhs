import System.IO
import System.Environment

data Node =
    Loop [Node] |
    Operator Char

instance Show Node where
    show (Loop nodes) = '[' : (foldl (++) "" (map show nodes)) ++ "]"
    show (Operator c) = [c]
    
type Program = [Node]

validChars :: String
validChars =  "+-<>,.[]"

operators :: String
operators = "+-<>,."

isOperator :: Char -> Bool
isOperator x = x `elem` operators

dropComment :: String -> String
dropComment "" = ""
dropComment ('#': rest) = ""
dropComment (c: rest) = c : (dropComment rest)


dropComments :: [String] -> [String]
dropComments = map dropComment

dropNonBfChars :: [String] -> [String]
dropNonBfChars = map dropChars
    where
        dropChars = filter isValidChar
        isValidChar c = c `elem` validChars


parseChar :: String -> (Node, String)
parseChar (c: rest)
    | isOperator c = (Operator c, rest)
    | otherwise = parseLoop rest

parseLoop :: String -> (Node, String)
parseLoop str = ((Loop nodes), rest)
    where
        (nodes, rest) = parseLoop' str
        parseLoop' (']': rest) = ([], rest)
        parseLoop' str = (node : restOfLoop, restOfInput)
            where
                (node, afterFirstChar) = parseChar str
                (restOfLoop, restOfInput) = parseLoop' afterFirstChar


parse :: String -> Program
parse "" = []
parse str = node : restOfProgram
    where
        (node, afterFirstChar) = parseChar str
        restOfProgram = parse afterFirstChar

astToStr :: Program -> String
astToStr program = foldl (++) "" (map show program)

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (head args)

    let allLines = lines contents
        noComments = dropComments allLines
        bfChars = dropNonBfChars noComments
        flattened = foldl (++) "" bfChars
        ast = parse flattened

    putStrLn (astToStr ast)

