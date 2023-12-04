-- compile: ghc -Wall --make Main.hs
-- run: Main < input.txt

main :: IO ()
main = interact (readInput)

-- reading input
readInput :: String -> String
readInput input = unlines (map readInputLines (lines input))

-- reading in the input line by line
readInputLines :: String -> String
readInputLines line = unlines (map (\word -> "= " ++ word)  (splitLine line))

-- splitting input at spaces and returning list
splitLine :: String -> [String]
splitLine sLine = words sLine

-- creating data type for regular expression tree
data RegexTree = Empty | Epsilon | Character Char | Alternation RegexTree RegexTree 
            | OneOrMore RegexTree | KleeneStar RegexTree | Optional RegexTree 
            | Concatentation RegexTree RegexTree
            deriving (Show)

isOperator :: Char -> Bool
isOperator op = op `elem` ['|', '+', '*', '?', '@', '\x2205', '\x03B5']

-- fillTree :: String -> RegexTree
