{-
 - Author: Ava Crocker, acrocker2021@my.fit.edu
 - Author: Max Caiazza, mcaiazza2021@my.fit.edu
 - Course: CSE 4250, Fall 2023
 - Project: Proj4, Tautology Checker
 - Language implementation: Glorious Glasgow Haskell Compilation System, version 8.4.3
 -}

-- compile: ghc -Wall --make Main.hs
-- run: Main < input.txt

--work on what was in the blog 
-- Nail down encoding problems
    -- UTF-8
    -- 

import Data.List (foldl)

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
data Regex = Empty | Epsilon | Leaf Char | Alternation Regex Regex
            | Plus Regex | KleeneStar Regex | Optional Regex
            | Concatenation Regex Regex
            deriving (Show)

isOperator  :: Char -> Bool
isOperator op = op `elem` ['|', '+', '*', '?', '@']

derive :: Regex -> Char -> Regex
derive Empty _ = Empty
derive Epsilon _ = Empty
derive (Leaf x') x
    | x == x' = Epsilon
    | otherwise = Empty
derive (Alternation r1 r2) x =
    --
derive (Plus r) x = 
    -- 
derive (KleeneStar r) x =
    --
derive (Optional r) x = 
    --
derive (Concatentation r1 r2) x =
    --
nullable :: Regex -> Bool
-- if regex = epsilon
-- used to compare input derivation to output
match :: Regex -> String -> Bool
match x str = nullable (foldl derive x str)

regexToTree :: String -> Regex
regexToTree = popTree []
  where
    popTree :: [Regex] -> String -> Regex
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