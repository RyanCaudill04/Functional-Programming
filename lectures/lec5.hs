data Tree a =
  Empty
  | Node a (Tree a) (Tree a)
  deriving (Show, Eq)


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

-- Doesn't work. 
-- initializeTree' :: (Ord a) => [a] -> Tree a
-- initializeTree' [] = Empty
-- initializeTree' [x] = leaf x
-- initializeTree' (x:xs) =
--   if x < head xs then Node x Empty (initializeTree' xs)
--   else Node x (initializeTree' xs) Empty

initializeTree' :: (Ord a) => [a] -> Tree a -> Tree a
initializeTree' [] tr = tr
initializeTree' (x:xs) tr = initializeTree' xs (insert x tr)
--   initializeTree' xs (Node x Empty Empty) 
-- initializeTree' (x:xs) (Node y l r)
--   | x < y =
--     initializeTree' xs (Node y (insert x l) r)
-- initializeTree' (x:xs) (Node y l r) | otherwise =
--     initializeTree' xs (Node y l (insert x r))

insert :: (Ord a) => a -> Tree a -> Tree a    
insert x Empty = leaf x
insert x (Node y l r) | x < y = Node y (insert x l) r
insert x (Node y l r) | otherwise = Node y l (insert x r)


-- Initialize a BST tree from an nonempty tree. 
initializeTree :: (Ord a) => [a] -> Tree a
initializeTree [] = Empty
initializeTree (x:xs) = insert x (initializeTree xs)

depthFirst :: Tree a -> [a]  
depthFirst Empty = []
depthFirst (Node x l r) =
  depthFirst l ++ depthFirst r ++ [x]

breathFirstHelper :: [Tree a] -> [a]
breathFirstHelper [] = []
breathFirstHelper (Empty:xs) = breathFirstHelper xs
breathFirstHelper (Node x l r : xs) =
  x : breathFirstHelper (xs ++ [l, r])

breathFirst :: Tree a -> [a]
breathFirst tr = breathFirstHelper [tr]
-- breathFirst (Node x l r) =
--  x : root l : root r : (left l) right l
--  x : (left l) : right l : left r : right r














  


  




  
