-- Data Types

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

tree :: Tree Int -- The type of the tree
tree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)


