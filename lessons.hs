-- Data Types

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

tree :: Tree Int -- The type of the tree
tree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

-- Records

-- this just doesn't has any semantics, it's just a tuple
-- data Person = Person String Int deriving (Show)

data Person = Person { name :: String, age :: Int } deriving (Show)

-- with which we get the following functions for free
name :: Person -> String
age :: Person -> Int

-- Type Classes

-- Type classes are like interfaces in Java
-- They define a set of functions that a type must implement
-- For example, the Eq type class defines the == and /= functions
-- The Ord type class defines the <, >, <=, >= functions
-- The Show type class defines the show function
-- The Read type class defines the read function
-- The Enum type class defines the succ and pred functions
-- The Bounded type class defines the minBound and maxBound functions
-- The Num type class defines the +, -, *, negate, abs, signum, fromInteger functions

data Temperature = C Float | F Float
	deriving (Show, Eq)

instance Eq Temperature where
  (==) (C x) (C y) = x == y
	(==) (F x) (F y) = x == y
	(==) (C x) (F y) = x == (y - 32) * 5 / 9
	(==) (F x) (C y) = (x - 32) * 5 / 9 == y


