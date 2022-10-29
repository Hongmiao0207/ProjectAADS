{-
  COMP4040 Project in Advanced Algorithms and Data Structures
    Autumn 2022

  Assignment 1 
     Red-Balck Trees

  Student Name: .....
  Student ID: ....

  Complete this Haskell file by providing definitions
  of the following functions (do not change their types):

    searchRB

    minRB
    maxRB

    isBST

    blackBalanced
    blackHeight

    insertRB
    deleteRB

  You are allowed to define any other auxiliary function you need.

-}

module RedBlack where

import Data.Maybe

-- Definition of the Red-Black Tree data structure

data Color = Red | Black
  deriving (Eq,Show)

data RBT a = LeafRB | NodeRB Color (RBT a) a (RBT a)
  deriving (Eq,Show)


-- Serching a key inside a red-black tree
--   return True if the key is found, False otherwise

searchRB :: Ord a => a -> RBT a -> Bool
searchRB x LeafRB = False
searchRB x (NodeRB _ left i right)
  | x > i = searchRB x right 
  | x < i = searchRB x left
  | otherwise = True

-- Minimum and maximum of red-black tree
--   return Nothing if the tree is empty

minRB :: RBT a -> Maybe a
minRB LeafRB = Nothing
minRB (NodeRB _ LeafRB x _) = Just x
minRB (NodeRB _ left x _) = minRB left

maxRB :: RBT a -> Maybe a
maxRB LeafRB = Nothing
maxRB (NodeRB _ _ x LeafRB) = Just x
maxRB (NodeRB _ _ x right) = maxRB right

-- Check if a tree satisfies the Binary Search Tree condition
--   (do not check other RBT conditions)
isBST :: Ord a => RBT a -> Bool
isBST tree = isJust $ inorderMax tree Nothing where
  inorderMax (NodeRB _ LeafRB i right) wrappedMax =
    case wrappedMax >>= \m -> pure (m < i) of
      Just False -> Nothing
      _  -> inorderMax right (Just i)
  inorderMax (NodeRB _ left i right) wrappedMax = 
    case inorderMax left wrappedMax >>= \m -> pure (m < i) of
      Just True -> inorderMax right (Just i)
      _ -> Nothing

-- Check the Black-balancing condition:
--     all paths have the same number of black nodes

-- blackBalanced :: RBT a -> Bool


-- Black height of a black-balanced tree, -1 if not black-balanced

blackHeight :: RBT a -> Int
blackHeight = blackHeight' 0
  where
    blackHeight' n LeafRB = n+1
    blackHeight' n (NodeRB Red l _ r) = blackHeight' n  l + blackHeight' n  r
    blackHeight' n (NodeRB Black l _ r) = blackHeight' n' l + blackHeight' n' r
      where
        n' = n + 1

-- Check if all Red-Black Tree conditions are satisfied
isRBT :: Ord a => RBT a -> Bool
isRBT LeafRB = True
isRBT (NodeRB Red l _ r)
 |isBlack l && isBlack r = isRBT l && isRBT r
 |otherwise = False
isRBT (NodeRB _ l _ r) = isRBT l && isRBT r

isBlack (NodeRB Black _ _ _) = True
isBlack LeafRB = True
isBlack _ = False
isRed = not . isBlack


makeBlack :: RBT a -> RBT a
makeBlack LeafRB = LeafRB
makeBlack (NodeRB _ left i right) = NodeRB Black left i right

makeRed :: RBT a -> RBT a
makeRed LeafRB = LeafRB
makeRed (NodeRB _ left i right) = NodeRB Red left i right

-- balance
makeBalance :: Color -> RBT a -> a -> RBT a -> RBT a
makeBalance Black (NodeRB Red (NodeRB Red a x b) y c) z d = NodeRB Red (NodeRB Black a x b) y (NodeRB Black c z d)
makeBalance Black (NodeRB Red a x (NodeRB Red b y c)) z d = NodeRB Red (NodeRB Black a x b) y (NodeRB Black c z d)
makeBalance Black a x (NodeRB Red (NodeRB Red b y c) z d) = NodeRB Red (NodeRB Black a x b) y (NodeRB Black c z d)
makeBalance Black a x (NodeRB Red b y (NodeRB Red c z d)) = NodeRB Red (NodeRB Black a x b) y (NodeRB Black c z d)
makeBalance color left value right = NodeRB color left value right

-- Insert a new element in a RBT, preserving the RBT properties
insertRB :: Ord a => a -> RBT a -> RBT a
insertRB x root = makeBlack $ insert' root                       
  where insert' LeafRB = NodeRB Red LeafRB x LeafRB
        insert' root@(NodeRB color left i right)
            | x > i = makeBalance color left i (insert' right)
            | x < i = makeBalance color (insert' left) i right
            | otherwise = root

balance :: RBT a -> RBT a
balance (NodeRB Black (NodeRB Red (NodeRB Red a x b) y c) z d) = NodeRB Red (NodeRB Black a x b) y (NodeRB Black c z d)
balance (NodeRB Black (NodeRB Red a x (NodeRB Red b y c)) z d) = NodeRB Red (NodeRB Black a x b) y (NodeRB Black c z d)
balance (NodeRB Black a x (NodeRB Red (NodeRB Red b y c) z d)) = NodeRB Red (NodeRB Black a x b) y (NodeRB Black c z d)
balance (NodeRB Black a x (NodeRB Red b y (NodeRB Red c z d))) = NodeRB Red (NodeRB Black a x b) y (NodeRB Black c z d)
balance t@(NodeRB c x l r) = t

balanceL :: RBT a -> RBT a 
balanceL (NodeRB Black (NodeRB Red a x b) y r)                    = NodeRB Red (NodeRB Black a x b) y r
balanceL (NodeRB Black l y (NodeRB Black a z b))                  = balance $ NodeRB Black l y (NodeRB Red a z b)
balanceL (NodeRB Black l y (NodeRB Red (NodeRB Black a u b) z c)) = NodeRB Red (NodeRB Black l y a) u (balance $ NodeRB Black b z (makeRed c))

balanceR :: RBT a -> RBT a 
balanceR (NodeRB Black l y (NodeRB Red a x b))                    = NodeRB Red l y (NodeRB Black a x b)
balanceR (NodeRB Black (NodeRB Black a z b) y r)                  = balance $ NodeRB Black (NodeRB Red a z b) y r
balanceR (NodeRB Black (NodeRB Red c z (NodeRB Black a u b)) y r) = NodeRB Red (balance $ NodeRB Black (makeRed c) z a) u (NodeRB Black b y r)

app :: RBT a -> RBT a -> RBT a
app LeafRB t = t
app t LeafRB = t 
app (NodeRB Red a x b) (NodeRB Red c y d) = 
  case app b c of
    NodeRB Red b' z c' -> NodeRB Red (NodeRB Red a x b') z (NodeRB Red c' y d)
    s -> NodeRB Red a x (NodeRB Red s y d)
app (NodeRB Black a x b) (NodeRB Black c y d) =
  case app b c of
    NodeRB r b' z c' -> NodeRB Red (NodeRB Black a x b') z (NodeRB Black c' y d)
    s -> balanceL $ NodeRB Black a x (NodeRB Black s y d)
app (NodeRB Red a x b) t = NodeRB Red a x (app b t)
app t (NodeRB Red a x b) = NodeRB Red (app t a) x b

-- Delete an element from a RBT, preserving the RBT properties
deleteRB :: Ord a => a -> RBT a -> RBT a
deleteRB x = makeBlack . del
  where
    del LeafRB = LeafRB
    del t@(NodeRB _ l y r) | x < y     = delL t
                           | x > y     = delR t
                           | otherwise = app l r
    delL (NodeRB _ l@(NodeRB Black _ _ _) y r) = balanceL $ NodeRB Black (del l) y r
    delL (NodeRB _ l y r)                      = NodeRB Red (del l) y r
    delR (NodeRB _ l y r@(NodeRB Black _ _ _)) = balanceR $ NodeRB Black l y (del r)
    delR (NodeRB _ l y r)                      = NodeRB Red l y (del r)