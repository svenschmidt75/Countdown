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
root      [x] subtrees = let sta = Leaf x : subtrees in
                         let sts = Leaf (-x) : subtrees in
                         sta ++ sts
root (x:y:xs) subtrees = let sta = map (\z -> Node Add (Leaf x) z) add_subtrees in
                         let stb = map (\z -> Node Add (Leaf (-x)) z) add_subtrees in
                         sta ++ stb
                         where
                            add_subtrees = root (y:xs) subtrees
