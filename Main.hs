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

--work on what was in the blog 
-- Nail down encoding problems
    -- UTF-8
    -- 
module Main where

import Data.List (foldl)
import Debug.Trace

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
-- match :: Regex -> String -> Bool
-- match x str = nullable (foldl derive x str)

match :: Regex -> String -> Bool
match x str = trace ("Input: " ++ show x ++ ", " ++ show str) $
              trace ("Output: " ++ show result) result
  where
    result = nullable (foldl derive x str)


-- checking if regex is epsilon: checking nullability
nullable :: Regex -> Bool
nullable Empty = False
nullable Epsilon = True
nullable (Leaf _) = False
nullable (Alternation r1 r2) = nullable r1 || nullable r2
nullable (Plus r) =  nullable r
nullable (KleeneStar _) = True
nullable (Optional _) = True
nullable (Concatenation r1 r2) = nullable r1 && nullable r2

-- defining the data type for regular expressions
data Regex = Empty | Epsilon | Leaf Char | Alternation Regex Regex
            | Plus Regex | KleeneStar Regex | Optional Regex
            | Concatenation Regex Regex
            deriving (Show)

-- deriving each eqn for regex
derive :: Regex -> Char -> Regex
derive Empty _ = Empty
derive Epsilon _ = Empty
derive (Leaf x') x
    | x == x' = Epsilon
    | otherwise = Empty
derive (Alternation r1 r2) x =
    -- eqn for alternation
    derive r1 x `Alternation` derive r2 x
derive (Plus r) x = 
    -- eqn for plus
    derive r x `Concatenation` (derive r x `KleeneStar`)
derive (KleeneStar r) x =
    -- eqn for kleene
    derive r x `Concatenation` (derive r x `KleeneStar`)
derive (Optional r) x = 
    -- eqn for optional
    derive r x `Alternation` Epsilon
derive (Concatenation r1 r2) x =
    -- eqn for concat
    if not (nullable r1) then derive r1 x `Concatenation` r2 
    else derive r1 x `Alternation` derive r2 x `Concatenation` r2

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