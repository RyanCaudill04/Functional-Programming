{-# LANGUAGE FlexibleInstances #-}
-- Note: Your file must be free of typing errors. If your file can not
-- be loaded into ghci, then you will get 0 point. Please read the instructions
-- for each problem carefully. Failure to follow the instructions may result in
-- 0 point.


-- Recall the following definition of rose tree. 
data RoseTree a = Node a [RoseTree a] deriving (Show)


-- Problem 1. (2 points) Define an instance to make RoseTree an instance of
-- Eq class.


-- Recall the following definition of monoid. 
class MyMonoid a where
  mempty' :: a
  mappend' :: a -> a -> a

-- Problem 2. (2 points). Complete the following
-- instance definition.
instance MyMonoid (a -> a) where 


-- Consider the following MyFoldable type class.
class MyFoldable t where
  myfoldMap :: (MyMonoid b) => (a -> b) -> t a -> b


-- Maybe is an example of Foldable.
instance MyFoldable Maybe where
  myfoldMap f Nothing = mempty'
  myfoldMap f (Just x) = f x 

-- Problem 3. (2 points) Define list as instance of MyFoldable
instance MyFoldable [] where


-- Problem 4. (2 points) Define myfoldr using only myfoldMap.
-- Note: Your definition must be a one-liner.  
-- Hint: note that (b -> b) is already an instance of MyMonoid

myfoldr :: (MyFoldable t) => (a -> b -> b) -> b -> t a -> b
myfoldr = undefined

-- Your definitions must be so that the following code calculate the sum
-- of a int list.
sum' :: [Int] -> Int
sum' = myfoldr (\ x r -> x+r) 0  




