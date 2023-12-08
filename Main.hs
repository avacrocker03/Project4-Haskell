{-
 - Author: Ava Crocker, acrocker2021@my.fit.edu
 - Author: Max Caiazza, mcaiazza2021@my.fit.edu
 - Course: CSE 4250, Fall 2023
 - Project: Proj4, Tautology Checker
 - Language implementation: Glorious Glasgow Haskell Compilation System, version 8.4.3
 -}

 {-
 - Citations
 - https://betterprogramming.pub/how-to-take-the-derivative-of-a-regular-expression-explained-2e7cea15028d
 - https://wiki.haskell.org/Haskell
 -}

-- compile: ghc -Wall --make Main.hs
-- run: Main < input.txt

module Main where
    
main :: IO ()
main = interact readInput

-- reading input
readInput :: String -> String
readInput input = unlines (map readInputLines (lines input))

-- reading in the input line by line
readInputLines :: String -> String
readInputLines line =
    -- splitting the line by space & putting in list
    let wordsList = words line
        -- creating tree with first index of input list
        tree = regexToTree [] (head wordsList) 
        matched = match (formatRegex tree) (head wordsList) (wordsList!!1) -- storing result from comparing inputs
        strOut  = boolToString matched -- converting bool result to string
    in init (unlines [strOut]) -- printing output
    
-- converts bool values to string so they can be printed
boolToString :: Bool -> String
boolToString True  = "yes" -- formatting for output
boolToString False = "no" -- formatting for output

-- used to compare input derivation to output
match :: Regex -> String -> String -> Bool
match tree _ "ε" = nullable tree -- when epsilon present in eqn
match tree _ str = nullable (foldl derive tree str)

-- checking if regex is epsilon: checking nullability
nullable :: Regex -> Bool
nullable EmptySet = False
nullable Epsilon = True
nullable (Leaf _) = False
nullable (Alternation r1 r2) = nullable r1 || nullable r2
nullable (Plus r) =  nullable r
nullable (KleeneStar _) = True
nullable (Optional _) = True
nullable (Concatenation r1 r2) = nullable r1 && nullable r2

-- defining the data type for regular expressions
data Regex  = EmptySet | Epsilon | Leaf Char | Alternation Regex Regex | Plus Regex
              | KleeneStar Regex | Optional Regex | Concatenation Regex Regex
              deriving (Show)

-- deriving each eqn for regex
derive :: Regex -> Char -> Regex
derive EmptySet _ = EmptySet
derive Epsilon _ = EmptySet
derive (Leaf c) x = if c == x then Epsilon else EmptySet 
derive (Alternation r1 r2) x = Alternation (derive r1 x)  (derive r2 x)
derive (Plus r) x       = Concatenation (derive r x) (KleeneStar r)
derive (KleeneStar r) x = Concatenation (derive r x) (KleeneStar r)
derive (Optional r) x   = derive r x
derive (Concatenation r1 r2) x =
    if not (nullable r1) then Concatenation (derive r1 x) r2 
    else Alternation (Concatenation (derive r1 x) r2) (derive r2 x)

--Fixing the [regex] to regex problem
formatRegex :: [Regex] -> Regex
formatRegex (r:_) = r
formatRegex []  = EmptySet -- handling empty cases



-- Makes the tree from a given regex list and a string
regexToTree :: [Regex] -> String -> [Regex]
regexToTree stack (x:rest) = if isOperator x then regexToTree (operatorToRegex stack x) rest else regexToTree (charToRegex x:stack) rest
regexToTree stack _ = stack 

-- checking if the character is an operator
isOperator  :: Char -> Bool
isOperator op = op `elem` ['|', '+', '*', '?', '@']

--Gives back the regex for non operator characters
charToRegex :: Char -> Regex
charToRegex 'ε' = Epsilon 
charToRegex '∅' = EmptySet
charToRegex  c  = Leaf c

--Converts the char to a regex function
operatorToRegex :: [Regex] -> Char -> [Regex]
operatorToRegex (r2:r1:rest) '|' = (Alternation r1 r2):rest
operatorToRegex (r2:r1:rest) '@' = (Concatenation r1 r2):rest
operatorToRegex (r:rest)     '+' = (Plus r):rest
operatorToRegex (r:rest)     '*' = (KleeneStar r):rest
operatorToRegex (r:rest)     '?' = (Optional r):rest
operatorToRegex [] _ = [] -- handling empty cases
operatorToRegex _   _   = [] -- handling empty cases