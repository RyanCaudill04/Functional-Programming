-- Currying and lambda expression

add :: (Int, Int) -> Int
add (x, y) = x + y

-- Curried version of addition, which
-- is the preferred way of writing function
-- in Haskell 
add' :: Int -> Int -> Int
add' x y = x + y

-- lambda expression
add'' :: Int -> Int -> Int
add'' = (\ x y -> x + y)


-- \ x -> \ y -> x + y


-- Note that Int -> Int -> Int is also a shorthand for
-- Int -> (Int -> Int), i.e., '->' is a right-associative
-- operator.

-- Partial application. 
-- add' 1 

-- Recursion and pattern matching in Haskell

-- List constructors: (:) (called 'cons') and [] (empty list). 
listHead :: [a] -> a
listHead (x:xs) = x

listTail :: [a] -> [a]
listTail (x:xs) = xs

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- using case expression
myLength' :: [a] -> Int
myLength' l =
  case l of
    [] -> 0
    x:xs -> 1 + myLength' xs
    

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : (append xs ys)

-- using case expression
append' :: [a] -> [a] -> [a]
append' xs ys =
  case xs of
    [] -> ys
    x:xs' -> x : append' xs' ys 

-- More list functions

-- sumList :: [Int] -> Int
-- sumList = undefined

zip' :: [a] -> [b] -> [(a, b)]
zip' [] [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys
zip' (x:xs) [] = []
zip' [] (y:ys) = []

zip'' :: [a] -> [b] -> [(a, b)]
zip'' [] ys = []
zip'' (x:xs) [] = []
zip'' (x:xs) (y:ys) = (x,y) : zip'' xs ys

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a] -> [a]
myReverse' [] ys = ys 
myReverse' (x:xs) ys = myReverse' xs (x:ys)

-- Higher-order functions 
myMap :: (a -> b) -> [a] -> [b]
myMap f []  = []
myMap f (x:xs) =  f x : myMap f xs
