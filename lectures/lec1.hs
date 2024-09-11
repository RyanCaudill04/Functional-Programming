-- Haskell: being lazy with class and A
-- Youtube: Simon Peyton-Jones: Escape from the ivory tower: the Haskell journey
-- https://www.youtube.com/watch?v=re96UgMk6GQ


main :: IO ()
main = do
  putStrLn "Hello World!"
  putStrLn "Hello World twice!"
   

-- List notation in haskell

-- [a] a list of type a

{- 
list1 :: [Int]
list1 = [1,2,3]
-}

-- Haskell string is just a list of characters. 
list2 :: [Char]
list2 = ['a', 'b', 'c']

-- emptyList :: [a]
emptyList = []


-- List destructors: head, tail. 

-- common list functions: !!, take, drop, length, ++.

-- Tuple
tuple1 :: (Int, Char)
tuple1 = (1, 'a')

tuple2 :: (Int, Char, Bool)
tuple2 = (1, 'a', True)

tuple3 :: ([Int], Char)
tuple3 = ([1,2,3], 'a')

example1 :: [(Int, String)]
example1 = [(1, "Alice"), (1, "Albert"), (2, "Bob"), (3, "Clare")]

-- Tuple destructors: fst, snd












