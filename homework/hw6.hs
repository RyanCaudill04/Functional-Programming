{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

-- Recall the following definition of
-- a monad transformer type class. 
class MonadTrans t where
  lift :: (Monad m) => m a -> (t m) a

-- Recall that the Identity data type is a monad.  
data Identity a = Id {runId :: a}

instance Functor Identity where
  fmap f (Id x) = Id (f x)

instance Applicative Identity where
  pure x = Id x
  (Id f) <*> (Id x) = Id $ f x
  
instance Monad Identity where
  return :: a -> Identity a
  return = Id
  (>>=) :: Identity a -> (a -> Identity b) -> Identity b
  (Id x) >>= f = f x

-- Consider the following definition of the
-- Maybe monad transformer, MaybeT. 
data MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

-- Problem 1. (2 points). Show MaybeT m is an
-- instance of Functor. 
instance (Functor m) => Functor (MaybeT m) where

-- Problem 2. (3 points). Show MaybeT m is an
-- instance of Applicative. 
instance (Monad m) => Applicative (MaybeT m) where
    
    
-- Problem 3. (4 points). Show MaybeT m is an
-- instance of Monad. 
instance Monad m => Monad (MaybeT m) where
      
-- Recalled the following MonadPlus type class.       
class Monad m => MonadPlus m where
   mzero :: m a
   mplus :: m a -> m a -> m a

-- Problem 4. (4 points). Show (MaybeT m) is an
-- instance of MonadPlus. 

instance (Monad m) => MonadPlus (MaybeT m) where

type MyMaybe a = MaybeT Identity a

runMyMaybe x = runId (runMaybeT x)

test1 :: MyMaybe Int
test1 = (return 10) `mplus` mzero

-- Note that your implementation should
-- satisfy the following. 
-- runMyMaybe test1 == Just 10         

-- Problem 5. (2 points). Define the following safe division
-- for MyMaybe. 
safeDiv :: Int -> Int -> MyMaybe Int 
safeDiv = undefined


-- Consider the following data type Exp. 
data Exp =
  Base Int
  | Add Exp Exp
  | Div Exp Exp
  | Mul Exp Exp
  | Minus Exp Exp
  deriving (Show)

-- Problem 6. (4 points). Define the evaluation function eval. 
eval :: Exp -> MyMaybe Int
eval = undefined
-- Note: Your evaluation must satisfy the following. 
-- runMyMaybe $ eval (Div (Base 10) (Base 0))  == Nothing
-- runMyMaybe $ eval (Div (Base 10) (Base 2))  == Just 5
  

    
    

