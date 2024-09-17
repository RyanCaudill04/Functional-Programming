{-# LANGUAGE AllowAmbiguousTypes #-}

-- Custom monads
class (Functor m) => MyMonad m where
  join' :: m (m a) -> m a
  return' :: a -> m a
  bind' :: m (m a) -> (m a -> m b) -> m b
  bind' c f = join (fmap f c)

--instance MyMonad [] where
  --join' :: [[a]] -> [a]
  --join' = concat
  --return' :: a -> [a]
  --return' x = [x]

------------------------------------------

-- More natural monads use "bind"

--bind' :: m (m a) -> (m a -> m b) -> m b
--bind' c f = join(fmap f c)

--join' :: (Monad m) => m(m a) -> m a
--join' c = bind c (\ n -> n)

instance MyMonad [] where 
  bind' :: [a] -> (a -> [b]) -> [b]
  bind' [] f = []
  bind' (x:xs) f = f x ++ bind' xs f

instance MyMonad Maybe where
  return' = Just
  bind' Nothing f = Nothing
  bind' (Just x) f = f x

