-- primes = [x | x <- [2 .. ], and [x 'mod' y /= 0 | y <- [2..x-1]]]

-- fibonacci sequence methods
fib :: Int ->  Int -> Int -> Int
fib 0 x y = x
fib 1 x y = y
fib n x y | n > 1 = fib (n-1) y (x+y)

-- entire sequence
-- use take n fibs to get first n elements
fibs :: [Integer]
fibs = 0:1: zipWith (+) fibs (tail fibs)

-- trees
data Tree a = 
  Empty
  | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

tree0 :: Tree Int
tree0 = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)

leaf :: a -> Tree a
leaf x = Node x Empty Empty

height :: Tree a -> Int
height Empty = 0
height (Node x l r) = 1 + max (height l) (height r)

