module ExpressionTree where

data Tree = Node Op Tree Tree
          | Leaf Int
          deriving (Show)

data Op = Add
        | Sub
        | Mul
        | Div
        deriving (Show)

{- Input:
 - 1. List of integers to create a syntax tree for
 - 2. List of syntax trees generated, initially empty
 - Output:
 - List of all generated syntax trees
-}
root :: [Int] -> [Tree] -> [Tree]
root       []        _ = error "Should never happen"
root      [x] subtrees = let sta = Leaf x : subtrees in
                         let sts = Leaf (-x) : subtrees in
                         sta ++ sts
root (x:y:xs) trees = let sta1 = map (\z -> Node Add (Leaf x) z) subtrees in
                      let sta2= map (\z -> Node Add (Leaf (-x)) z) subtrees in
                      let stb1= map (\z -> Node Sub (Leaf x) z) subtrees in
                      let stb2= map (\z -> Node Sub (Leaf (-x)) z) subtrees in
                      let stc1= map (\z -> Node Mul (Leaf x) z) subtrees in
                      let stc2= map (\z -> Node Mul (Leaf (-x)) z) subtrees in
                      let std1= map (\z -> Node Div (Leaf x) z) subtrees in
                      let std2= map (\z -> Node Div (Leaf (-x)) z) subtrees in
                      sta1 ++ sta2 ++ stb1 ++ stb2 ++ stc1 ++ stc2 ++ std1 ++ std2
                      where
                        subtrees = root (y:xs) trees
