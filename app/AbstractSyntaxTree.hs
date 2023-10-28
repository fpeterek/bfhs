module AbstractSyntaxTree
( Node ( Loop, Operator )
, getNodes
, Program
, astToStr
) where


data Node =
    Loop { getNodes :: [Node] } |
    Operator Char

instance Show Node where
    show (Loop nodes) = '[' : (foldl (++) "" (map show nodes)) ++ "]"
    show (Operator c) = [c]
    
type Program = [Node]

astToStr :: Program -> String
astToStr program = foldl (++) "" (map show program)
