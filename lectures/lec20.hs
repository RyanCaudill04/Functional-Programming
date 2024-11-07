{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

-- In order to use quickcheck library,
-- you will need to do: cabal install QuickCheck --lib
-- in the command line. 
import Test.QuickCheck
import Control.Monad
-- import System.Random
-- cabal install random --lib
import Data.List hiding (insert, lookup, delete)
import Prelude hiding (lookup, delete)

-- Quickcheck's Arbitrary class.

data Color = Red | Blue | Green deriving (Show)

instance Arbitrary Color where
  arbitrary :: Gen Color
  arbitrary = elements [Red, Blue, Green]

-- Some useful combinators for generators
-- generate $ choose (0,1)
-- generate $ elements ['a','e','i','o','u']
-- generate $ (vector 100 :: Gen [Int])
-- generate $ frequency [(99, elements [True]), (1, elements [False])]
-- generate $ shuffle [0 .. 10]

prop_replicate ::  Int -> Int -> Int -> Bool
prop_replicate n i x =
    replicate n x !! i == x


-- A better property using forAll combinator. 
prop_replicate'' ::  Int -> Property
prop_replicate'' x =
  forAll (elements [1 .. 100])
  (\ n -> forAll (choose (0, n-1))
    (\ i -> replicate n x !! i == x))


data List a = Nil | Cons a (List a) deriving (Show)

length' :: Num a1 => List a2 -> a1
length' (Nil) = 0
length' (Cons a as) = 1 + length' as


cons' :: Gen a -> Gen (List a) -> Gen (List a)
cons' x y = do
  a <- x
  as <- y
  return (Cons a as)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary :: Gen (List a)
  arbitrary = frequency [(9, cons' arbitrary arbitrary),
                         (1, return Nil)]

data Tree a =
  Empty
  | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

tree0 :: Tree Int
tree0 =
  Node (Node Empty 1 Empty) 2 (Node Empty 3 Empty)

node' :: Gen (Tree a) -> Gen a -> Gen (Tree a) -> Gen (Tree a)
node' l x r = do
  left <- l
  a <- x
  Node left a <$> r

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary :: Gen (Tree a)
  arbitrary = sized sizedAb
    -- frequency [(2, node' arbitrary arbitrary arbitrary),
    --                      (1, elements [Empty])]


sizedAb :: (Arbitrary a) => Int -> Gen (Tree a)
sizedAb 0 = return Empty
sizedAb n =  node'  (sizedAb (n `div` 2)) arbitrary (sizedAb (n `div` 2))




