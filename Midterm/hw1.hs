-- Note: Your file must be free of typing errors. If your file can not
-- be loaded into ghci, then you will get 0 point. Please read the instructions
-- for each problem carefully. Failure to follow the instructions may result in
-- 0 point.


-- Problem 1. Write down definitions that have the following types;
-- it does not matter what the definitions actually do as long as they are type correct. Note that
-- you cannot use 'undefined' in your definitions, and your definitions must terminate when evaluated
-- with an argument. 

-- (1) 1 point.
d1 :: a -> (a, a)
d1 a = (a,a)


-- (2) 1 point.
d2 :: a -> a 
d2 a = a

-- (3) 1 point.
d3 :: a -> b -> a 
d3 a b = a


-- (4) 1 point.
d4 :: (a -> b) -> a -> b
d4 f a = f a 

-- (5) 1 point.
d5 ::  (a -> (b -> c)) -> ((a -> b) -> (a -> c))
d5 = undefined

-- (6) 1 point.
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a,b)

-- (7) 1 point.
uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a,b) = f a b

-- Problem 2 (2 points): Use pattern matching and recursion to define the
-- following 'myDrop' function. 
-- It drops the first n elements from a list.
-- We assume the input number is always greater or
-- equaled to 0. 

myDrop :: Int -> [a] -> [a]
myDrop 0 x = x
myDrop y [] = []
myDrop y (x:xs) = myDrop (y-1) xs

-- For your convenient, your implementation
-- of myDrop should have the following
-- behavior from the interpreter.

-- > myDrop 3 [1,2,3,4,5]
-- [4,5]
-- > myDrop 3 []
-- []
-- > myDrop 3 [1,2]
-- []
-- > myDrop 0 [1,2]
-- [1,2]
-- > myDrop 4 [1,2]
-- []

-- Problem 3 (2 points): Use pattern matching and recursion to define the following 'myConcat' function. 
-- It concatenates a list of list into a list.
-- You may use the built-in append function (++) from Haskell. 
myConcat :: [[a]] -> [a]
myConcat (x:xs) = x ++ myConcat xs
myConcat [] = []

-- Your function should at least satisfy the following
-- test case. 
-- > myConcat [[1,2], [3,4], [5,6]]
-- [1,2,3,4,5,6]


-- Problem 4 (2 points): Use pattern matching and recursion to define
-- the following 'duplicate' function. It produce a list with n identical elements.
-- We may assume the input number is always greater
-- or equaled to 0. 
duplicate :: Int -> a -> [a]
duplicate 0 _ = []
duplicate x y = [y] ++ duplicate (x-1) y

-- For example, 
-- > duplicate 3 'a'
-- "aaa"
-- > duplicate 3 '1'
-- "111"
-- > duplicate 3 1
-- [1,1,1]
-- > duplicate 0 1
-- []

-- Problem 5 (2 points):
-- Use pattern matching and recursion to define
-- the following 'myElem' function that decides if an input is an element of a list.
-- Note that the 'Eq a' type class is there so you can use the function (==) for equality comparison. 
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem y (x:xs) = if y == x then True
  else myElem y xs

-- For example, 
-- > myElem 1 [1,2,3,4]
-- True
-- > myElem 5 [1,2,3,4]
-- False    


-- Problem 6 (2 points):
-- Recall the following definition of myfoldr and myFilter function. 
myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f y [] = y
myfoldr f y (x:xs) = f x (myfoldr f y xs)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x:xs) =
  if f x then x : myFilter f xs
  else myFilter f xs

-- Define another version of myFilter' that uses myfoldr without using
-- pattern matching and recursion. 
myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' f xs = myfoldr (\x acc -> if f x then x:acc else acc) [] xs
