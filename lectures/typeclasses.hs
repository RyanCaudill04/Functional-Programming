tree0 :: Tree Int
tree0 =
  Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)

leaf :: a -> Tree a
leaf x = Node x Empty Empty

tree1 =
  Node 1 (Node 2 (leaf 4) (leaf 5)) (leaf 3)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Empty = Empty
mapTree f (Node x l r) =
  Node (f x) (mapTree f l) (mapTree f r) 


-- Motivation
data Color = Blue | Red | Green deriving (Show, Read)

colorEq :: Color -> Color -> Bool
colorEq Blue Blue = True
colorEq Red Red = True
colorEq Green Green = True
colorEq _ _ = False

-- This is too much work, for each list of new data type,
-- we would need a new function for comparing equality. 
listColorEq :: [Color] -> [Color] -> Bool
listColorEq [] [] = True
listColorEq (x:xs) (y:ys) = colorEq x y && listColorEq xs ys
listColorEq _ _ = False

listBoolEq :: [Bool] -> [Bool] -> Bool
listBoolEq = undefined


-- Problems of doing arithmetic. We want to use the same
-- multiplication symbol '*' to multiply different kind of numbers. (1 * 2)  (2.5 * 3.2)

class MyEq a where
  myEq :: a -> a -> Bool

notEq :: (MyEq a) => a -> a -> Bool
notEq x y = not (myEq x y)

instance MyEq Color where
  myEq Blue Blue = True
  myEq Red Red = True
  myEq Green Green = True
  myEq _ _ = False  

instance MyEq Bool where
  myEq True True = True
  myEq False False = True
  myEq _ _ = False

instance MyEq Int where
  myEq x y = x == y
  
-- instance that has assumptions, recursive instance   
instance (MyEq a) => MyEq [a] where
   myEq [] [] = True
   myEq (x:xs) (y:ys) = myEq x y && myEq xs ys
   myEq _ _ = False

instance (MyEq a) => MyEq (Maybe a) where
  myEq Nothing Nothing = False
  myEq (Just x) (Just y) = myEq x y
  myEq _ _ = False


data Tree a =
  Empty
  | Node a (Tree a) (Tree a)
  deriving (Show, Eq)
  
instance (MyEq a) => MyEq (Tree a) where   
  myEq Empty Empty = True
  myEq (Node x l r) (Node y l' r') =
    myEq x y && myEq l l' && myEq r r'


class (MyEq a) => MyOrd a where
  greater :: a -> a -> Bool
  geq :: a -> a -> Bool
  geq x y = greater x y || myEq x y

instance MyOrd Bool where
  greater True False = True
  greater _ _ = False
  -- We can overwrite the default geq method here. 
  geq x y = False


-- More useful type classes.
-- Show, Num.



class MyNum a where
  add :: a -> a -> a
  mul :: a -> a -> a
  negate :: a -> a

-- This is how haskell see it. 
data DictionaryNum a =
  Dict (a -> a -> a) (a -> a -> a) (a -> a) 

add' :: DictionaryNum a -> (a -> a -> a)
add' (Dict f g h) = f

mul' :: DictionaryNum a -> (a -> a -> a)
mul' (Dict f g h) = g

negate' :: DictionaryNum a -> (a -> a)
negate' (Dict f g h) = h

instance MyNum Int where
  add = addInt 
  mul = addMul
  negate = negateInt 

-- This is how Haskell see it. 
dictInt = Dict addInt addMul negateInt

-- When evaulating 'add 2 (3 :: Int)'
-- Haskell will just evaluate (add' dictInt) 2 3  --> addInt 2 3 --> 5

addInt :: Int -> Int -> Int
addInt x y = x + y

addMul :: Int -> Int -> Int
addMul x y = x * y

negateInt :: Int -> Int
negateInt x = -x 


instance MyNum Float where
  add x y = x + y
  mul x y = x * y
  negate x = -x

square :: MyNum a => a -> a
square x = mul x x

squares :: (MyNum a, MyNum b, MyNum c) => (a, b, c) -> (a, b, c)
squares (x, y, z) = (square x, square y, square z)












