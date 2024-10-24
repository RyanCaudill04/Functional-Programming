{-# LANGUAGE InstanceSigs #-}
import Data.Char (isSpace)
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (P p) =
    P $ \ s -> fmap (\ (x, s') -> (f x, s')) (p s)
    
instance Applicative Parser where

data Parser a = P (String -> Maybe (a,String))

instance Monad Parser where
  return :: a -> Parser a
  return x = P $ \ s -> Just (x, s)
  
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (P p) >>= f =
    P $ \ s -> case p s of
                 Nothing -> Nothing
                 Just (x, s') -> let P g = f x
                                 in g s'

-- A parser that always fail
zero :: Parser a
zero = P $ \ s -> Nothing

-- Check if it is the end of input string. 
eof :: Parser ()
eof = P $ \ s -> if null s then Just ((), s) else Nothing


parse :: String -> Parser a -> Maybe (a, String)
parse s (P p) = p s 

-- Parse an item
item :: Parser Char
item = P $ \ s -> case s of
                    [] -> Nothing
                    x:xs -> Just (x, xs)


-- A determinstic combinator that tries from left to right.    
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = P $ \ s -> case parse s p1 of
                         Nothing -> parse s p2
                         Just (x, s') -> Just (x, s')

            
-- Done with plumbing. Now we are ready to define some
-- useful parser combinators.

-- Parse a charactor that satisfies the condition.
sat :: (Char -> Bool) -> Parser Char
sat f = do
  x <- item
  if f x then return x
    else zero

-- Parse a charactor.
char :: Char -> Parser Char
char c = sat (\ x -> x == c)

-- Recursive parser combinators. Parse an input string
-- String = [Char]
string :: String -> Parser String
string [] = return []
string (x:xs) = do
  y <- char x
  ys <- string xs
  return (y:ys)

-- Apply a parser zero or more times. 
many :: Parser a -> Parser [a]
many p =
  do{
    x <- p;
    xs <- many p;
    return (x:xs)} <|> return []

-- Apply a parser one or more times. 
many1 :: Parser a -> Parser [a]
many1 p = do 
  x <- p
  xs <- many p
  return (x:xs)
  
-- sepBy p sep parses zero or more occurrences of p,
-- separated by sep. Returns a list of values returned by p.
sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep =
  do{ x <- p;
      xs <- many (sep >> p);
      return (x:xs)
    } <|> return []
  
sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = 
  do{ x <- p;
      xs <- many (sep >> p);
      return (x:xs)
    }

-- A simple CSV parser
csv :: Parser [[String]]
csv = do
  ls <- many line
  eof
  return ls



line :: Parser [String]
line = do
  cs <- content `sepby1` comma
  (char '\n' >> return ()) <|> eof
  return cs

comma :: Parser Char
comma = char ','

-- parse a charactor as long as it is not one of the charactor
-- in the input string.
noneOf :: [Char] -> Parser Char
noneOf str = do
  c <- item
  if elem c str then zero
    else return c

content :: Parser String
content = many1 (noneOf ",;.\n")

stringWithSep :: Char -> [String] -> String
stringWithSep c [] = []
stringWithSep c [a] = a
stringWithSep c (x:xs) = x ++ [c] ++ stringWithSep c xs

rmSpaces :: String -> String
rmSpaces [] = []
rmSpaces (x:xs) = 
  if isSpace x then rmSpaces xs 
  else x : rmSpaces xs