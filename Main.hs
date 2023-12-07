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

import Data.List (foldl)

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
        tree = regexToTree (head wordsList)
        matched = match tree (wordsList!!1)
        strOut  = boolToString matched
    in unlines [strOut, show tree, show (wordsList!!1), show (head wordsList)]
    
-- converts bool values to string
boolToString :: Bool -> String
boolToString True  = "yes"
boolToString False = "no"

-- checking if the character is an operator
isOperator  :: Char -> Bool
isOperator op = op `elem` ['|', '+', '*', '?', '@']

-- used to compare input derivation to output
match :: Regex -> String -> Bool
match x str = nullable (foldl derive x str)

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
data Regex = EmptySet 
            | Epsilon 
            | Leaf Char 
            | Alternation Regex Regex
            | Plus Regex 
            | KleeneStar Regex 
            | Optional Regex
            | Concatenation Regex Regex
            deriving (Show)

-- deriving each eqn for regex
derive :: Regex -> Char -> Regex
derive EmptySet x = EmptySet
derive Epsilon x = EmptySet
derive (Leaf c) x = if c == x then Epsilon else EmptySet
derive (Alternation r1 r2) x = Alternation (derive r1 x)  (derive r2 x)
derive (Plus r) x       = Concatenation (derive r x) (KleeneStar r)
derive (KleeneStar r) x = Concatenation (derive r x) (KleeneStar r)
derive (Optional r) x   = derive r x
derive (Concatenation r1 r2) x =
    if not (nullable r1) then Concatenation (derive r1 x) r2 
    else Alternation (Concatenation (derive r1 x) r2) (derive r2 x)

-- creating tree data structure
regexToTree :: String -> Regex
regexToTree = popTree []
  where
    popTree :: [Regex] -> String -> Regex
    popTree stack [] = head stack
    popTree stack (x:xs)
        -- checking if operator
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