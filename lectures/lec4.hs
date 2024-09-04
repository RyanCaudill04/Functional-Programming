-- Laziness of Haskell

take' :: Int -> [a] -> [a]
take' n xs | n == 0 = []
take' n [] | n > 0 = []
take' n (x:xs) | n > 0 = x : take' (n-1) xs
  
inf = [0 .. ]

inf' :: Int -> [Int]
inf' n = n : inf' (n+1) 

list3 = take' 5 inf


-- Primes
-- prime :: Int -> Bool

primes :: [Integer]
primes =
  [x | x <- [2 .. ],  and [ not (x `mod` y == 0) | y <- [2 .. x-1]] ] 

-- fibonacci:
-- 0, 1, 1, 2, 3, 5 ...
-- A slow fib 
fib :: Int -> Int
fib n | n == 0 = 0
fib n | n == 1 = 1
fib n | n > 1 = fib (n-1) + fib (n-2)

-- fib 100 -> fib 99 + fib 98 -> fib 98 + fib 97 + fib 97 + fib 96


-- A tail recursive fib.
fib' :: Integer -> Integer -> Integer -> Integer
fib' 0 x y = x
fib' 1 x y = y
fib' n x y | n > 1 = fib' (n-1) y (x + y)


-- 0, 1, 1, 2, 3, 5 ... 
-- 1, 1, 2, 3, 5 ...
-- ---------------------------
-- 1, 2, 3, 5, ...


fibs :: [Integer]
fibs = 0:1: zipWith (+) fibs (tail fibs) 


-- Algebraic data types
data List a =
  Nil
  | Cons a (List a)
  deriving (Show, Eq)

-- append
append :: List a -> List a -> List a 
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

data Pair a b = P a b
  deriving (Show, Eq)

data Maybe' a =
  Nothing'
  | Just' a 
  deriving (Show, Eq)

lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' k [] = Nothing
lookup' k ((k', v):xs) | k == k' = Just v
lookup' k ((k', v):xs) | otherwise = lookup' k xs

data Either' a b =
  Left' a
  | Right' b 
  deriving (Show, Eq)

lookup'' :: Eq a => a -> [(a, b)] -> Either String b
lookup'' k [] = Left "404"
lookup'' k ((k', v):xs) | k == k' = Right v
lookup'' k ((k', v):xs) | otherwise = lookup'' k xs


data Tree a =
  Empty
  | Node a (Tree a) (Tree a)
  deriving (Show, Eq)


tree0 :: Tree Int
tree0 = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)


leaf :: a -> Tree a
leaf x = Node x Empty Empty

tree1 =
  Node 1 (Node 2 (leaf 4) (leaf 5)) (leaf 3)

height :: Tree a -> Int
height Empty = 0
height (Node x l r) = 1 + max (height l) (height r)



