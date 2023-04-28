-- Excercises
-- 1. Create a function rev that reverses a list

rev :: [a] -> [a]
rev [] = []
rev = foldl (flip (:)) [] 
-- Equivalent to 
-- rev = foldl (\acc x -> x : acc) []

-- 2, Create a function prefixes taht returns all the prefixes of a given list

prefixes :: [a] -> [[a]]
prefixes = foldr (\x acc -> [x] : map (x:) acc) []
-- map (x:) acc is the same as map (\xs -> x:xs) acc 
-- or map (\xs -> [x] ++ xs) acc
-- map is applied to the accumulator, which is a list of lists
-- so map (x:) acc is a list of lists, where each list is the prefix x:xs

-- 3. Given a set a data points (list of float tuples), give the lagrange interpolation of them (https://en.wikipedia.org/wiki/Lagrange_polynomial)
-- Lagrange interpolation is a polynomial that passes through all the points

-- xs is the list of points, x is the point to interpolate
lagrange :: [(Float, Float)] -> Float -> Float 
lagrange xs x = sum $ map (\(xj, yj) -> yj * l xj) xs
		where l xj = product $ map (\(xk, _) -> (x - xk) / (xj - xk)) $ filter (\(xk, _) -> xk /= xj) xs
-- alternative
lagrange xs x = foldl (\acc (xj, y) -> acc + (y * l xj)) 0 xs
	where 
		l xj = foldl (
			\acc (xk, _) -> 
				if xk == xj 
					then acc * (x - xk) / (xj - xk) 
				else acc
			) 1 xs

-- 4. Implement a Trie (Prefix Tree) data structure (https://en.wikipedia.org/wiki/Trie)

data Trie a = Leaf a | Node a [Trie a] deriving (Show)
-- Preorder traversal
-- foldtrie folds the elements of a trie in Preorder
-- f is the function to apply to the accumulator and the current element
-- acc is the accumulator
foldtrie :: (b -> a -> b) -> b -> Trie a -> b
foldtrie f acc (Leaf x) = f acc x
foldtrie f acc (Node x xs) = foldl f' (f acc x) xs
	where f' acc x = foldtrie f acc x

