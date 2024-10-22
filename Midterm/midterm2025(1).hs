-- Midterm
-- Note: 
-- 1. Please make sure your file is free of typing errors.
-- 2. Please read the instructions of the problem carefully,
-- failure to follow the instructions may result in zero point. 


-- Problem 1 (2 points). Use pattern matching and
-- recursion to define the following
-- conjunctions function such that it returns
-- the conjunctions (logical and) of all the booleans in
-- a list.
-- E.g. conjunction [True, False] == False
-- conjunction [True, True] == True
-- conjunction [] == True
conjunctions :: [Bool] -> Bool
conjunctions [] = True
conjunctions (x:xs) = x && conjunctions xs



-- Problem 2 (2 points). Define conjunctions' function  
-- using only foldr. Your conjunctions' function
-- must behave the same as the conjunctions function in
-- Problem 1. 
conjunctions' :: [Bool] -> Bool
conjunctions' x = foldr (&&) True x

-- Problem 3 (2 points) Define the following
-- sequence' function.
-- Note: Your must *not* use the
-- builtin sequence function. 
sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (x:xs) = do
  x' <- x
  xs' <- sequence' xs
  return (x':xs')

sequence'' :: Monad m => [m a] -> m [a]
sequence'' [] = return []
sequence'' (x:xs) = x >>= \ x' -> sequence'' xs >>= \ xs' -> return (x':xs')


-- e.g. sequence' [Just 2, Just 3] == Just [2, 3]
-- sequence' [Just 2, Just 3, Nothing] == Nothing



-- Problem 4 (2 points) Define the following
-- function mapM' using sequence'.
-- Note: Your must *not* use the
-- builtin mapM function. 
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f a = sequence' (map f a)



-- e.g.  mapM'' (\ x -> if even x then Nothing else Just x) [1,3, 5] == Just [1,3,5]

-- mapM' (\ x -> if even x then Nothing else Just x) [1,2,3] == Nothing

-- Problem 5 (2 points) Define the following foldM' function
-- such that 
-- foldM' f a1 [x1, x2, ..., xm]
-- == 
-- do
--   a2 <- f a1 x1
--   a3 <- f a2 x2
--   ...
--   f am xm.
-- Note: Your must *not* use the
-- builtin "foldM" function. 
foldM' :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldM' f b [] = return b
foldM' f b (x:xs) = f b x >>= \b' -> foldM' f b' xs

foldM'' :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldM'' f b [] = return b
foldM'' f b (x:xs) = do
  b' <- f b x
  foldM'' f b' xs


-- e.g., foldM' (\ r x -> Just (r + x)) 0 [1, 2, 3, 4]
-- == Just 10

-- > foldM (\ r x -> do{print r; return (r+x)}) 0 [1, 2, 3, 4]
-- 0
-- 1
-- 3
-- 6
-- 10

-- Problem 6 (2 points). Define mapM'' using foldM.
-- Your mapM'' should behave the same as mapM'. 
mapM'' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM'' f xs = foldM' (\ r x -> f x >>= \y -> return (r ++ [y])) [] xs
