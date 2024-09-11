-- Lists are monoids
-- (List, append, [])

class MyMonoid a where
  mempty' :: a
  mappend' :: a -> a -> a

instance MyMonoid Bool where
  mempty' = True
  mappend' x y = x && y

instance MyMonoid Integer where
  mempty' = 0
  mappend' x y = x + y

instance MyMonoid [a] where
  mempty' = []
  mappend' = (++)

-- Functors