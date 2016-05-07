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
 - 1. List of integers to create an expression tree for
 - 2. List of expression trees generated, initially empty
 - Output:
 - List of all generated expression trees
 -
 - Note: Some of the generated expression trees are equivalent in the sense
 -       that the resulting algebraic expression is the same.
 -       More so, since we later use all permutations of these numbers,
 -       we will create tons of redundant expression trees. This is to be
 -       cleaned up at a later stage.
 -       Example:
 -       The expression tree for +1+3*(-4) is equivalent to the one generated
 -       from +1-3*4. Currently, we generate both.
-}
generateExpressionTree :: [Int] -> [Tree]
generateExpressionTree       [] = error "Should never happen"
generateExpressionTree       xs = generateExpressionTree_internal xs []

generateExpressionTree_internal :: [Int] -> [Tree] -> [Tree]
generateExpressionTree_internal       []        _ = error "Should never happen"
generateExpressionTree_internal      [x] subtrees = let sta = Leaf x    : subtrees in
                                                    let sts = Leaf (-x) : subtrees in
                                                    sta ++ sts
generateExpressionTree_internal (x:y:xs) trees =
                      let sta1 = map (\z -> Node Add (Leaf x   ) z) subtrees in
                      let sta2 = map (\z -> Node Add (Leaf (-x)) z) subtrees in
                      let stb1 = map (\z -> Node Sub (Leaf x   ) z) subtrees in
                      let stb2 = map (\z -> Node Sub (Leaf (-x)) z) subtrees in
                      let stc1 = map (\z -> Node Mul (Leaf x   ) z) subtrees in
                      let stc2 = map (\z -> Node Mul (Leaf (-x)) z) subtrees in
                      let std1 = map (\z -> Node Div (Leaf x   ) z) subtrees in
                      let std2 = map (\z -> Node Div (Leaf (-x)) z) subtrees in
                      sta1 ++ sta2 ++ stb1 ++ stb2 ++ stc1 ++ stc2 ++ std1 ++ std2
                      where
                        subtrees = generateExpressionTree_internal (y:xs) trees

evaluateExpressionTree :: Tree -> Int
evaluateExpressionTree (Leaf x) = x
evaluateExpressionTree (Node Add l r) = evaluateExpressionTree l + evaluateExpressionTree r
evaluateExpressionTree (Node Sub l r) = evaluateExpressionTree l - evaluateExpressionTree r
evaluateExpressionTree (Node Mul l r) = evaluateExpressionTree l * evaluateExpressionTree r
evaluateExpressionTree (Node Div l r) = evaluateExpressionTree l `div` evaluateExpressionTree r

describeExpressionTree :: Tree -> String
describeExpressionTree (Leaf x)
    | x < 0     = "(" ++ show x ++ ")"
    | otherwise = show x
describeExpressionTree (Node Add l r) = describeExpressionTree l ++ "+" ++ describeExpressionTree r
describeExpressionTree (Node Sub l r) = describeExpressionTree l ++ "-" ++ describeExpressionTree r
describeExpressionTree (Node Mul l r) = describeExpressionTree l ++ "*" ++ describeExpressionTree r
describeExpressionTree (Node Div l r) = describeExpressionTree l ++ "/" ++ describeExpressionTree r

hasInvalidDivision :: Tree -> Bool
hasInvalidDivision (Leaf _)                     = False
hasInvalidDivision (Node Div (Leaf x) (Leaf y)) = m * y /= x
                                                  where
                                                   m  = quot x y
hasInvalidDivision (Node _ l r)                 = hasInvalidDivision l || hasInvalidDivision r

main :: IO ()
main = do
        let tree = generateExpressionTree [1, 2, 3]
        print tree
        let tree2 = filter (not . hasInvalidDivision) tree
        print tree2
        let tree3 = map evaluateExpressionTree tree2
        print tree3
