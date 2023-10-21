module Parser
( parse
) where

import AbstractSyntaxTree


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


parseClean :: String -> Program
parseClean "" = []
parseClean str = node : restOfProgram
    where
        (node, afterFirstChar) = parseChar str
        restOfProgram = parseClean afterFirstChar

parse :: String -> Program
parse str =
    let allLines   = lines str
        noComments = dropComments allLines
        bfChars    = dropNonBfChars noComments
        flattened  = foldl (++) "" bfChars
    in
        parseClean flattened
