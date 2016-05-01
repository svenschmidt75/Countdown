module Countdown where

rotateInternal :: [Int] -> Int -> [[Int]]
rotateInternal [] _ = []
rotateInternal q@(x:xs) n
    | n > 0     = q : rotateInternal (xs ++ [x]) (n - 1)
    | otherwise = []

rotate :: [Int] -> [[Int]]
rotate xs = rotateInternal xs (length xs)

prefix :: [[Int]] -> Int -> [[Int]]
prefix xs x = map (x:) xs

permutations :: [Int] -> [[Int]]
permutations [] = [[]]
permutations  q = let rotated = rotate q in
                  concatMap (\(x:xs) -> prefix (permutations xs) x) rotated
