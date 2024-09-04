-- Note: Your file must be free of typing errors. If your file can not
-- be loaded into ghci, then you will get 0 point. Please read the instructions
-- for each problem carefully. Failure to follow the instructions may result in
-- 0 point.


-- Problem 0. (2 points). Define an infinite list of 2's powers. 
-- e.g. take 10 xs = [1,2,4,8,16,32,64,128,256,512]
-- Note: your solution must be a one-liner. 
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
xs :: [Integer]
xs = undefined 

-- Bonus Problem (2 points). Define an infinte list that contains all the rows in Pascal's triangle (https://en.wikipedia.org/wiki/Pascal%27s_triangle).
-- E.g., take 10 ys == [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1],[1,6,15,20,15,6,1],[1,7,21,35,35,21,7,1],[1,8,28,56,70,56,28,8,1],[1,9,36,84,126,126,84,36,9,1]]
-- Note: your solution must be a one-liner. 
ys :: [[Integer]]
ys = undefined

-- A rose tree is a tree that can have arbitray many of subtrees.
-- In Haskell, we can define rose tree by the following.

data RoseTree a = Node a [RoseTree a]
 deriving (Eq, Show)

-- Problem 1 (2 points). Represent the following folder structure as a rose tree.
-- 
-- my-app
-- |- node_modules
-- |- public
-- |  |- favicon.ico
-- |  |- index.html
-- |  |- robots.txt
-- |
-- |- src
-- |  |- index.css
-- |  |- index.js
-- |- .gitignore
-- |- package.json
-- |- README.md
-- 

tree1 :: RoseTree String
tree1 = Node "my-app" [
  (Node "node_modules" []),
  (Node "public" [
    (Node "favicon.ico" []),
    (Node "index.html" []),
    (Node "robots.txt" [])
  ]),
  (Node "src" [
    (Node "index.css" []),
    (Node "index.js" [])
  ]),
  (Node ".gitignore" []),
  (Node "package.json" []),
  (Node "README.md" [])
  ]


-- Problem 2 (2 points). Define a map function for rose tree.
mapRose :: (a -> b) -> RoseTree a -> RoseTree b
mapRose = undefined


-- Problem 3 (2 points). Define a function that returns the total number of nodes in a rose tree
nodes :: RoseTree a -> Int
nodes = undefined


-- Problem 4 (2 points). Define a function that does a depth-first traversal on a rose tree.
-- E.g., depthFirst tree1 == ["node_modules","favicon.ico","index.html","robots.txt","public","index.css","index.js","src",".gitignore","package.json","README.md","my-app"] 
depthFirst :: RoseTree a -> [a]
depthFirst = undefined



-- Problem 5 (2 points). Define a function that does a breadth-first traversal on a rose tree.
-- E.g., breadthFirst tree1 == ["my-app","node_modules","public","src",".gitignore","package.json","README.md","favicon.ico","index.html","robots.txt","index.css","index.js"]
breadthFirst :: RoseTree a -> [a]
breadthFirst = undefined




