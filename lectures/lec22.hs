{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

-- In order to use quickcheck library,
-- you will need to do: cabal install QuickCheck --lib
-- in the command line. 
import Test.QuickCheck
import Control.Monad
import Data.List hiding (insert, lookup, delete)
import Prelude hiding (lookup, delete)


data List a = Nil | Cons a (List a) deriving (Show)

length' Nil = 0
length' (Cons x xs) = 1 + length' xs

cons' :: Gen a -> Gen (List a) -> Gen (List a)
cons' x y = do
  a <- x
  as <- y
  return (Cons a as)
  
instance Arbitrary a => Arbitrary (List a) where
  arbitrary :: Gen (List a)
  arbitrary = frequency
    [(9, cons' arbitrary arbitrary),
    (1, elements [Nil])]

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
  right <- r
  return $ Node left a right
  
instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary :: Gen (Tree a)
  arbitrary = sized sizedAb
    -- frequency [(1, return Empty),
    --            (2, node' arbitrary arbitrary arbitrary)
    --           ]
    

  
sizedAb :: (Arbitrary a) => Int -> Gen (Tree a)
sizedAb 0 = return Empty
sizedAb n =
  node'  (sizedAb (n `div` 2)) arbitrary (sizedAb (n `div` 2))

sizedAb' :: (Arbitrary a) => Int -> Gen (Tree a)
sizedAb' 0 = return Empty
sizedAb' n = frequency [
  (1, return Empty), 
  (2, node'  (sizedAb' (n `div` 2)) arbitrary (sizedAb' (n `div` 2)))
  ]



minNode :: Ord a => Tree a -> Maybe a
minNode Empty = Nothing
minNode (Node Empty x r) = Just x
minNode (Node l x r) = minNode l

maxNode :: Ord a => Tree a -> Maybe a
maxNode Empty = Nothing
maxNode (Node l x Empty) = Just x
maxNode (Node l x r) = maxNode r


isBST :: Ord a => Tree a -> Bool
isBST Empty = True
isBST (Node l x r) =
  case (maxNode l, minNode r) of
    (Just y, Just z) -> y < x && x < z && isBST l && isBST r
    (Nothing, Just z) -> x < z && isBST r
    (Just y, Nothing) -> y < x && isBST l
    (Nothing, Nothing) -> True
    

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty
insert x (Node l y r) | x == y = Node l y r
insert x (Node l y r) | x > y = Node l y (insert x r)
insert x (Node l y r) | x < y = Node (insert x l) y r

initialize :: Ord a => [a] -> Tree a
initialize [] = Empty
initialize (x:xs) = insert x (initialize xs)

prop :: [Int] -> Bool
prop xs = isBST $ initialize xs


sizedBST :: Int -> Int -> Int -> Gen (Tree Int)
sizedBST lo hi 0 = return Empty
sizedBST lo hi n =
  frequency
  [ (1, return Empty),
    (3, helper)
  ]
  where helper :: Gen (Tree Int)
        helper = do
          x <- choose (lo , hi)
          if lo < x && x < hi then do  
            l <- sizedBST lo (x-1) (n `div` 2)
            r <- sizedBST (x+1) hi (n `div` 2)
            return $ Node l x r
            else if abs (hi - lo) <= 1 then
                   return Empty
                 else helper




prop_sizedBST_isBST =
  forAll (sizedBST 0 40 8) (\ x -> isBST x)

  

lookup :: Ord a => a -> Tree a -> Maybe a
lookup x Empty = Nothing
lookup x (Node l y r) =
  if x == y then Just y
  else if x > y then lookup x r
       else lookup x l


deleteMinNode :: Tree a -> (a, Tree a)
deleteMinNode Empty = error "impossible"
deleteMinNode (Node Empty y r) = (y, r)
deleteMinNode (Node l y r) =
  let (min, l') = deleteMinNode l
  in (min, Node l' y r)

delete :: Ord a => a -> Tree a -> Tree a
delete x Empty = Empty 
delete x (Node Empty y Empty) | x == y = Empty
delete x (Node Empty y Empty) | otherwise = Node Empty y Empty
delete x (Node l y Empty) | x == y = l
delete x (Node l y Empty) | x < y = Node (delete x l) y Empty
delete x (Node l y Empty) | x > y = Node l y Empty
delete x (Node Empty y r) | x == y = r
delete x (Node Empty y r) | x < y = Node Empty y r
delete x (Node Empty y r) | x > y = Node Empty y (delete x r)
delete x (Node l y r) | x == y =
                        let (z, r') = deleteMinNode r
                        in 
                          Node l z r'
delete x (Node l y r) | x < y = Node (delete x l) y r
delete x (Node l y r) | x > y = Node l y (delete x r)



prop_delete_valid =
  forAll (sizedBST 0 40 8) (\ tr -> forAll (choose (0, 40)) (\ i -> isBST $ delete i tr))




prop_insert_valid = undefined


prop_delete_lookup = undefined

prop_insert_lookup = undefined

prop_insert_delete = undefined






