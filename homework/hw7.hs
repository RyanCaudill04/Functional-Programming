{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
import Test.QuickCheck
import Control.Monad
import Data.List hiding (union, intersect, insert)

-- Consider the following data type definition.

data Stack a = Stack [a] deriving (Show, Eq)

-- We can implement Stack by using list. 

-- Problem 1 (4 points). Define the following stack operations. 
emptyStack :: Stack a
emptyStack = Stack []

push :: a -> Stack a -> Stack a
push x (Stack a) = Stack (a ++ [x])

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, emptyStack)
pop (Stack [a]) = (Just a, emptyStack)
pop (Stack xs) = (Just (last xs), Stack (init xs))

size :: Stack a -> Int
size (Stack []) = 0
size (Stack (x:xs)) = 1 + size (Stack xs)

-- Problem 2. (4 points) Think of 4 properties of your stack operations.
-- Your implementations must pass quickcheck. 
instance Arbitrary a => Arbitrary (Stack a) where
  arbitrary = do
    Stack <$> arbitrary

prop_emptyStack :: Bool
prop_emptyStack = size emptyStack == 0

prop_sizePush :: a -> Stack a -> Bool
prop_sizePush x stack = size (push x stack) == size stack + 1

prop_sizePop :: Stack a -> Bool
prop_sizePop stack = case pop stack of
  (Nothing, _) -> size stack == 0
  (Just x, newStack) -> size newStack == size stack - 1

prop_pushPop :: Eq a => a -> Stack a -> Bool
prop_pushPop x stack = 
  let (result, newstack) = pop (push x stack) in
  result == Just x && size stack == size newstack


-- Consider the following data type. 
data Set a = Set [a] deriving (Show)

-- We can implement the following functions on finite sets.

-- Problem 3. (2 points) Define the set insertion function.                
insert :: (Eq a) => a -> Set a -> Set a
insert x (Set xs) = if x `elem` xs
  then Set xs
  else Set (xs ++ [x])

emptySet :: Set a
emptySet = Set []

member :: (Eq a) => a -> Set a -> Bool
member x (Set xs) = x `elem` xs

-- Problem 4. (2 points) Define Set a as an instance of Eq class.
instance (Eq a) => Eq (Set a) where
  (==) :: Eq a => Set a -> Set a -> Bool
  (==) (Set x) (Set y) = unionList x y == y && intersectList x y == x


-- Note that Set [1,2,3] == Set [3, 2,1] should return True.

sizeSet :: Set a -> Int
sizeSet (Set xs) = length xs

-- Problem 5. (2 points) Define the following set union function.
union :: (Eq a) => Set a -> Set a -> Set a
union (Set a) (Set b) = Set $ unionList a b

unionList :: Eq a => [a] -> [a] -> [a]
unionList [] ys = ys
unionList (x:xs) ys = if x `elem` ys
  then unionList xs ys
  else x : unionList xs ys

-- Problem 6. (2 points) Define the following set intersection function.
intersect :: (Eq a) => Set a -> Set a -> Set a
intersect (Set a) (Set b) = Set $ intersectList a b

intersectList :: Eq a => [a] -> [a] -> [a]
intersectList [] _ = []
intersectList (x:xs) ys = if x `elem` ys
  then x : intersectList xs ys
  else intersectList xs ys


-- Problem 7. (5 points) Please formulate 5 properties to check the
-- implementation of union and intersection. 
instance Arbitrary a => Arbitrary (Set a) where
  arbitrary = do
    Set <$> arbitrary

prop_unionSelf :: Eq a => Set a -> Bool
prop_unionSelf a = union a a == a

prop_intersectSelf :: Eq a => Set a -> Bool
prop_intersectSelf a = intersect a a == a

prop_unionEmpty :: Eq a => Set a -> Bool
prop_unionEmpty a = union a emptySet == a

prop_intersectEmpty :: Eq a => Set a -> Bool
prop_intersectEmpty a = intersect a emptySet == emptySet

prop_unionAssoc :: Eq a => Set a -> Set a -> Set a -> Bool
prop_unionAssoc a b c = union a (union b c) == union b (union a c) && union c (union a b) == union a (union b c)
