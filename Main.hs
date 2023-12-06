-- compile: ghc -Wall --make Main.hs
-- run: Main < input.txt

--work on what was in the blog 
-- Nail down encoding problems
    -- UTF-8
    -- 
main :: IO ()
main = interact readInput

-- reading input
readInput :: String -> String
readInput input = unlines (map readInputLines (lines input))

-- reading in the input line by line
readInputLines :: String -> String
readInputLines line =
    let wordsList = words line
        tree = regexToTree (head wordsList)
    in unlines (show tree : map ("= " ++) wordsList)

-- creating data type for regular expression tree
data RegexTree = Empty | Epsilon | Leaf Char | Alternation RegexTree RegexTree
            | Plus RegexTree | KleeneStar RegexTree | Optional RegexTree
            | Concatenation RegexTree RegexTree
            deriving (Show)

isOperator  :: Char -> Bool
isOperator op = op `elem` ['|', '+', '*', '?', '@']

regexToTree :: String -> RegexTree
regexToTree = popTree []
  where
    popTree :: [RegexTree] -> String -> RegexTree
    popTree stack [] = head stack
    popTree stack (x:xs)
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
          in popTree newStack xs
        | otherwise = popTree (Leaf x:stack) xs