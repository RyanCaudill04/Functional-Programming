{-# LANGUAGE OverloadedStrings #-}
-- In order to use quickcheck library,
-- you will need to do: cabal install QuickCheck --lib
-- in the command line. 
import Test.QuickCheck
import Control.Monad
import Data.List hiding (insert)


distance :: Int -> Int -> Int
distance x y = abs (y-x)

-- The distance between any number and itself is always 0
prop_dist_self :: Int -> Bool
prop_dist_self x = distance x x == 0

-- The distance between x and y is equal to the distance between y and x
prop_dist_symmetric :: Int -> Int -> Bool
prop_dist_symmetric x y = distance x y == distance y x


-- quickCheck :: Testable prop => prop -> IO ()
-- verboseCheck :: Testable prop => prop -> IO ()
-- withMaxSuccess :: Testable prop => Int -> prop -> Property

-- QuickCheck's Testable class.

-- what if we forget abs when defining distance. 


qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs



prop_idempotent :: [Int] -> Bool
prop_idempotent xs = qsort (qsort xs) == qsort xs

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [a] = True
sorted (x:y:xs) = if x <= y then sorted (y:xs) else False

