-- compile: ghc -Wall --make Main.hs
-- run: Main < input.txt

main :: IO ()
main = interact readInput

-- reading input
readInput :: String -> String
readInput input = unlines (map readInputLines (lines input))

-- reading in the input line by line
readInputLines :: String -> String
readInputLines line =
    let wordsList = splitLine line
        tree = convertToTree (head wordsList)
    in unlines (show tree : map ("= " ++) wordsList)

-- splitting input at spaces and returning list
splitLine :: String -> [String]
splitLine sLine = words sLine

-- creating data type for regular expression tree
data RegexTree = Empty | Epsilon | Leaf Char | Alternation RegexTree RegexTree
            | Plus RegexTree | KleeneStar RegexTree | Optional RegexTree
            | Concatenation RegexTree RegexTree
            deriving (Show)

isOperator :: Char -> Bool
isOperator op = op `elem` ['|', '+', '*', '?', '@']

-- fillTree :: String -> RegexTree

convertToTree :: String -> RegexTree
convertToTree = buildTree []
  where
    buildTree :: [RegexTree] -> String -> RegexTree
    buildTree stack [] = head stack
    buildTree stack (x:xs)
        | isOperator x =
            let newStack = case x of
                        '|' -> let (r2:r1:rest) = stack
                            in Alternation r1 r2:rest
                        '+' -> let (r:rest) = stack
                            in Plus r:rest
                        '*' -> let (r:rest) = stack
                            in KleeneStar r:rest
                        '?' -> let (r:rest) = stack
                            in Optional r:rest
                        '@' -> let (r2:r1:rest) = stack
                            in Concatenation r1 r2:rest
          in buildTree newStack xs
        | otherwise = buildTree (Leaf x:stack) xs
