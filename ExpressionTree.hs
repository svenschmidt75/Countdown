module ExpressionTree where

data Tree = Node Op Tree Tree
          | Leaf Int
          deriving (Show)

data Op = Add
        | Sub
        | Mul
        | Div
        deriving (Show)

root :: [Int] -> [Tree] -> [Tree]
root       []        _ = error "Should never happen"
root      [x] subtrees = Leaf x : subtrees
root (x:y:xs) subtrees = let add_subtrees = root (y:xs) subtrees in
                         map (\z -> Node Add (Leaf x) z) add_subtrees
