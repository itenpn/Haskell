module BinaryTree (
  Tree,
  leaf,
  insert,
  inTree,
  createTree
)
where

{-Declaration for Binary Tree-}
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

{-Returns a tree with with only one node-}
leaf :: a -> Tree a
leaf x = Node x Empty Empty

{-Inserts values into the tree. If the value already exists
 - then the function will not change anything-}
insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = leaf x
insert x (Node a left right)
  | x == a = Node x left right
  | x < a  = Node a (insert x left) right
  | x > a  = Node a left (insert x right)

{-Looks inside tree for an element-}
inTree :: (Ord a) => a -> Tree a -> Bool
inTree x Empty = False
inTree x (Node a left right)
  | x == a = True
  | x < a  = inTree x left
  | x > a  = inTree x right

{-Takes a list of Ord types and creates an
 - unsorted tree-}
createTree :: (Ord a) => [a] -> Tree a
createTree [] = Empty
createTree x = foldr insert Empty x
