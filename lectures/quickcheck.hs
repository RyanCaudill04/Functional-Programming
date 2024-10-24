import Test.QuickCheck
import Control.Monad
import Data.List hiding (insert)

distance :: Int -> Int -> Int
distance x y = abs (y - x)

prop_dist_self :: Int -> Bool
prop_dist_self x = distance x x == 0

prop_dist_symm :: Int -> Int -> Bool
prop_dist_symm x y = distance x y == distance y x