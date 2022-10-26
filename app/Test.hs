
data Color = Red | Black
data RBTree = Leaf | Node Color RBTree Key RBTree

flatten :: RBTree a -> [a]
flatten Leaf = []
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

ins :: RBTree → RBTree → RBTree
ins a Leaf = Node Red Leaf a Leaf
ins a t@(Node color t1 x t2)
        | a < x = balance color (ins a t1) x t2
        | a > x = balance color t1 x (ins a t2)
        | otherwise = t




