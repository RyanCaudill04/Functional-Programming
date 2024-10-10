{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses  #-}


class MonadTrans t where
  lift :: (Monad m) => m a -> (t m) a

-- t is monad transformer if
-- for any monad m, we have a monad (t m)

data Identity a = Id {runId :: a}


instance Functor Identity where
  fmap f (Id x) = Id (f x)

instance Applicative Identity where
  pure :: a -> Identity a
  pure = undefined

instance Monad Identity where
  return :: a -> Identity a
  return = Id
  (>>=) :: Identity a -> (a -> Identity b) -> Identity b
  (Id x) >>= f = f x

type State s a = StateT s Identity a
-- s -> (a, s)

type Parser a = StateT String Maybe a
-- String -> Maybe (a, String)


parse :: String -> Parser a -> Maybe (a, String)
parse s p = runStateT p s

-- Our first example of a monad transformer.
-- It gives us ability to add state to an existing
-- monad. 
data StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT g) =
    StateT $ \ x -> fmap (\ (y, s) -> (f y, s)) (g x)
    -- :: m (b, s)
    -- g :: s -> m (a, s)
    -- g x :: m (a, s)
    -- fmap_m :: (a -> b) -> m a -> m b
    -- fmap_m :: ((a, s) -> (b, s)) -> m (a, s) -> m (b, s)
  

instance (Applicative m) => Applicative (StateT s m) where

instance (Monad m) => Monad (StateT s m) where
  return :: a -> StateT s m a
  return x = StateT $ \ s -> return (x, s) -- :: m (a, s)
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT g) >>= f =
    StateT $ \ x -> g x >>= (\ (y, z) ->
                               let (StateT h) = f y
                               in h z
                            ) -- :: (a, s) -> m (b, s)
    -- y :: a
    -- z :: s                    
    -- x :: s
    -- g :: s -> m (a, s)
    -- g x :: m (a, s)
    -- f :: a -> StateT s m b
    -- h :: s -> m (b, s)

instance MonadTrans (StateT s) where
  lift :: (Monad m) => m a -> StateT s m a
  lift = undefined


