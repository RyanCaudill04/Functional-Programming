{-# LANGUAGE InstanceSigs #-}
class Monoid' a where
  mempty' :: a
  mappend' :: a -> a -> a

class MyFunctor f where
  my_fmap :: (a -> b) -> f a -> f b


instance MyFunctor Maybe where
  my_fmap :: (a -> b) -> Maybe a -> Maybe b
  my_fmap f Nothing = Nothing
  my_fmap f (Just x) = Just (f x)
  
instance MyFunctor [] where
  my_fmap :: (a -> b) -> [a] -> [b]
  my_fmap f [] = []
  my_fmap f (x:xs) = f x : my_fmap f xs

class (MyFunctor m) => MyMonad m where
  my_return :: a -> m a
  my_join :: m (m a) -> m a

instance MyMonad [] where
  my_return :: a -> [a]
  my_return x = [x]

  my_join :: [[a]] -> [a]
  my_join = concat

my_bind :: (MyMonad m) => m a -> (a -> m b) -> m b
my_bind x f = my_join (my_fmap f x)

join' :: (Monad' m) => m (m a) -> m a
join' c = bind' c (\ x -> x)  

class (MyFunctor m) => Monad' m where
  return' :: a -> m a
  bind' :: m a -> (a -> m b) -> m b



    

data Exp =
  Base Integer
  | Add Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  deriving (Show, Eq)

safeDiv :: Integer -> Integer -> Maybe Integer
safeDiv x y | y == 0 = Nothing
safeDiv x y | otherwise = Just (x `div` y)

eval :: Exp -> Maybe Integer
eval (Base i) = Just i
eval (Add e1 e2) =
  case eval e1 of
    Nothing -> Nothing
    Just e1' -> case eval e2 of
                  Nothing -> Nothing
                  Just e2' -> Just (e1' + e2')
eval (Mul e1 e2) =
  case eval e1 of
    Nothing -> Nothing
    Just e1' -> case eval e2 of
                  Nothing -> Nothing
                  Just e2' -> Just (e1' * e2')

eval (Div e1 e2) =
  case eval e1 of
    Nothing -> Nothing
    Just e1' -> case eval e2 of
                  Nothing -> Nothing
                  Just e2' -> safeDiv e1' e2'


exp1 = Add (Base 1) (Mul (Base 2) (Base 3))

exp2 = Add (Base 1) (Div (Base 2) (Base 0))                  

instance Monad' Maybe where
  return' :: a -> Maybe a
  return' x = Just x

  bind' :: Maybe a -> (a -> Maybe b) -> Maybe b
  bind' Nothing f = Nothing
  bind' (Just x) f = f x
                  

-- Do notation
-- y `bind` (\ x -> e)
-- will be translated to
-- x <- y
-- e
eval' :: Exp -> Maybe Integer
eval' (Base i) = return' i
eval' (Add e1 e2) = 
  eval' e1 `bind'`
  (\ x ->
      eval' e2 `bind'`
      (\ y -> return' (x + y))
  )
eval' (Mul e1 e2) = 
  eval' e1 `bind'`
  (\ x ->
      eval' e2 `bind'`
      (\ y -> return' (x * y))
  )
eval' (Div e1 e2) = 
  eval' e1 `bind'`
  (\ x ->
      eval' e2 `bind'`
      (\ y -> safeDiv x y)
  )

eval'' :: Exp -> Maybe Integer
eval'' (Base i) = return i
eval'' (Add e1 e2) = do
  x <- eval'' e1
  y <- eval'' e2
  return (x + y)

eval'' (Mul e1 e2) = do
  x <- eval'' e1
  y <- eval'' e2
  return (x * y)

eval'' (Div e1 e2) = do
  x <- eval'' e1
  y <- eval'' e2
  safeDiv x y

instance Monad' [] where
  return' :: a -> [a]
  return' x = [x]

  bind' :: [a] -> (a -> [b]) -> [b]
  bind' [] f = []
  bind' (x:xs) f = f x ++ bind' xs f

pairing' :: [a] -> [b] -> [(a, b)]
pairing' xs ys = [(a, b) | a <- xs, b <- ys]

pairing'' :: [a] -> [b] -> [(a, b)]
pairing'' xs ys = 
  xs >>= (\ x ->
             ys >>= (\ y -> return (x, y))
         )

pairing''' :: [a] -> [b] -> [(a, b)]
pairing''' xs ys = do
  x <- xs
  y <- ys
  return (x, y)

type State s a = s -> (a, s)














