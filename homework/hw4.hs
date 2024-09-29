{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
import GHC.Float (fabsDouble)
import System.Console.Terminfo (functionKey)
-- Note: Your file must be free of typing errors. If your file can not
-- be loaded into ghci, then you will get 0 point. Please read the instructions
-- for each problem carefully. Failure to follow the instructions may result in
-- 0 point.

-- The so-called "applicative functor" is
-- defined by the following type class. 
class (Functor f) => Applicative' f where
  pure' :: a -> f a
  app :: f (a -> b) -> f a -> f b

-- Problem 1 (2 points). Show that Maybe is an instance
-- of Applicative' by defining an instance declaration.

instance Applicative' Maybe where
  pure' = Just
  app (Just f) (Just a) = Just (f a)
  app f Nothing = Nothing
  app Nothing a = Nothing


-- Problem 2 (2 points). Show that list [] is an instance
-- of Applicative' by defining an instance declaration.

instance Applicative' [] where
  pure' a = [a]
  -- app fs xs = [f x | f <- fs, x <- xs]
  app fs xs = do
    f <- fs
    x <- xs
    pure' (f x)

-- Note: your definition must give the following
-- results. 
-- > pure' (+1) `app` [1,2,3,4]
-- [2,3,4,5]
-- > pure' (*) `app` [1,2,3,4] `app` [1,2]
-- [1,2,2,4,3,6,4,8]


-- Problem 3 (2 points). Define the following sequencing function
-- using only pattern matching, recursion, pure' and app.
sequenceA' :: Applicative' f => [f a] -> f [a]
sequenceA' [] = pure' []
sequenceA' (x:xs) = app (app (pure' (:) ) x) (sequenceA' xs)
-- sequenceA' (x:xs) = do
--  x' <- x
--  xs' <- sequenceA' xs
--  pure' (x':xs')

-- Your definition must give the following results. 
-- > sequenceA' [Just 1, Nothing, Just 2]
-- Nothing
-- > sequenceA' [Just 1, Just 3, Just 2]
-- Just [1,3,2]


-- Recall Monad type class.
class (Functor m) => Monad' m where
  return' :: a -> m a
  bind' :: m a -> (a -> m b) -> m b

-- Problem 4. (3 points). We can show a monad is also an applicative by the following.

pure'' :: (Monad' f) => a -> f a
pure'' = return'

app'' :: (Monad' f) => f (a -> b) -> f a -> f b
app'' func fa = func `bind'` \f -> fa `bind'` \a -> return' (f a)
-- app'' ff fa = do
--  f <- ff
--  a <- fa
--  return' (f a)