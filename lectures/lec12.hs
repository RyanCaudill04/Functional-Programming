{-# LANGUAGE InstanceSigs #-}
-- type State s a = s -> (a, s)

-- A better way is to define state as a data type. 
data State s a = S (s -> (a, s)) 

-- We won't talk about Applicative here, it is in the homework. 
instance Applicative (State s) where
  
instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (S g) =
    S (\ x -> let (y, s) = g x
              in (f y , s))

-- g :: s -> (a, s)
-- f :: a -> b
instance Monad (State s) where
  return :: a -> State s a
  return y = S (\ x -> (y, x))
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (>>=) (S g) f =
    S (\ x -> let (y, s) = g x
                  S g' = f y -- g' :: s -> (b, s)
                  (y', s') = g' s
              in (y', s')
      )
    -- g :: s -> (a, s)
    -- x :: s
    -- f :: a -> State s b
    

-- An example of using the State monad: labeling tree with numbers.



data Tree a = Leaf a | Node a (Tree a) (Tree a)
  deriving Show

tree1 = Node "f" (Node "e" (Leaf "c") (Leaf "d")) (Node "g" (Leaf "a") (Leaf "b"))


-- Do notation
-- x <- g y :: m a
label :: Tree a -> State Int (Tree Int)
label (Leaf x) = do
  c <- getState
  -- putState (c+1)
  modifyState (+1)
  return (Leaf c)

label (Node x l r) = do
  c <- getState
  putState (c+1)
  l' <- label l
  r' <- label r
  return (Node c l' r')


depth :: Tree a -> State [a] ()
depth (Leaf x) =
  modifyState (\ xs -> xs ++ [x])
  -- xs <- getState
  -- putState (xs++[x])
depth (Node x l r) = do
  depth l >> modifyState (\ xs -> xs ++ [x]) 
  depth r


seq' :: Monad m => m a -> m b -> m b
seq' m1 m2 = m1 >>= (\ x -> m2)

getState :: State s s
getState = S (\ x -> (x, x))

putState :: s -> State s ()
putState y = S $ \ x -> ((), y) 

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \ x -> ((), f x)

runState :: s -> State s a -> (a, s)
runState x (S g) = g x 


breadth :: State [Tree a] [a]
breadth = do
  trs <- getState
  case trs of
    [] -> return []
    Leaf x : xs -> do
      putState xs
      r <- breadth 
      return (x:r)
    Node x l r : xs -> do
      putState (xs ++ [l, r])
      y <- breadth 
      return (x:y)


-- IO monad.
-- We can think of IO as a very special kind of state monad.

-- type IO a = World -> (a, World)

-- It comes equipped with the following monadic methods.
-- return :: a -> IO a
-- (>>=) :: IO a -> (a -> IO b) -> IO b

-- Note that there does not exists a (runIO :: IO a -> a) function, because
-- 'World' is not accessible by the programmers. 
--
-- IO monad is equipped with a lot of
-- operations, e.g.,
-- getChar :: IO Char, putChar :: Char -> IO ().

sequenceIO :: [IO a] -> IO ()
sequenceIO [] = return ()
sequenceIO (x:xs) = 
  x >> sequenceIO xs

echo :: IO ()
echo = do
  c <- getChar
  if c == 'a' then 
    putChar 'a' >> putChar 'a'
    else putChar c >> echo


putString :: String -> IO ()
putString [] = return ()
putString (x:xs) = putChar x >> putString xs
  

putString' :: String -> IO ()
putString' str = sequenceIO $ map putChar str




