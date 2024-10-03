{-# LANGUAGE InstanceSigs #-}

import System.IO
import System.Environment

data Parser a = P (String -> Maybe (a, String))

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (P p) = 
    P $ \ s -> case p s of
      Nothing -> Nothing
      Just (a, s') -> Just (f a, s')

instance Monad Parser where
  return :: a -> Parser a
  return x = P $ \ s -> Just (x, s)
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (P p) >>= f = undefined


