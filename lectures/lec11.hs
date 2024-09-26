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

getState :: State s s
getState = S (\ x -> (x, x))

putState :: s -> State s ()
putState y = S $ \ x -> ((), y) 

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \ x -> ((), f x)

runState :: s -> State s a -> (a, s)
runState x (S g) = g x 


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

