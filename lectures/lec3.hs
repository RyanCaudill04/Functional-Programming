-- Uncomment the following flag if you want to warn
-- incomplete pattern matching. 
-- {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x:xs) =
  if f x then x : myFilter f xs
  else myFilter f xs

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' f [] = []
myFilter' f (x:xs) | f x = x : myFilter' f xs
myFilter' f (x:xs) | otherwise = myFilter' f xs
    
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] ys = []
zipWith' f (x:xs) [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- f :: Int -> Int
-- f x = x + 1
-- f x = x + 2

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs


-- We can use foldr to define a lot of functions that
-- we seen before.

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : (append xs ys)


myLength' :: [a] -> Int
myLength' l = myfoldr (\ x r -> r + 1) 0 l 

append' :: [a] -> [a] -> [a]
append' l1 l2 = myfoldr (\ x r -> x:r) l2 l1 


-- Higher-order functions 
myMap :: (a -> b) -> [a] -> [b]
myMap f []  = []
myMap f (x:xs) =  f x : myMap f xs

-- foldr 
myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f y [] = y
myfoldr f y (x:xs) = f x (myfoldr f y xs)

-- foldl
myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl f y [] = y
myfoldl f y (x:xs) = f (myfoldl f y xs) x

myMap' :: (a -> b) -> [a] -> [b]
myMap' f l = myfoldr (\ x r -> f x : r) [] l

-- List comprehension in Haskell

list1 = [ x  | x <- [1 .. 10], x `mod` 2 == 0]

list1' = filter (\ x -> x `mod` 2 == 0) [1..10] 

list2 = [ (x, y)  | x <- [1 .. 10], y <- [1.. 10]]

list2' = concat  $ map (\ x -> map (\ y -> (x, y)) [1..10]) [1..10]


-- Laziness of Haskell

take' :: Int -> [a] -> [a]
take' n xs | n == 0 = []
take' n [] | n > 0 = []
take' n (x:xs) | n > 0 = x : take' (n-1) xs
  
inf = [0 .. ]

inf' :: Int -> [Int]
inf' n = n : inf' (n+1) 

list3 = take' 5 inf





