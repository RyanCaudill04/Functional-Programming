{-# LANGUAGE InstanceSigs #-}
import Data.Char

-- Problem 0. (3 points) Define Parser as an instance for the
-- applicative. 
instance Applicative Parser where
  pure :: a -> Parser a
  pure a = P (\ s -> Just (a,s))
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) a b = do
    f <- a
    fx <- b
    pure (f fx)

-- Problem 1. (2 points). Define a token parser that first applies the
-- input parser, then consumes as many trailing whitespaces as possible, 
-- then finally it returns the value from the input parser.
-- Hint: you may find the function "isSpace" useful. 
token :: Parser a -> Parser a
token a = do
  x <- a
  many (sat isSpace)
  pure x

-- e.g.,
-- > parse "a     b" $ token (char 'a')
-- Just ('a',"b")

-- The following is just a helper function similar to
-- the "string" function, except it takes care of the trailing white spaces. 
symb :: String -> Parser String
symb cs = token (string cs)

-- Problem 2. (2 points). Define a parser that parses digits as
-- an Int. Note that must use the token function above to take care of the
-- whitespaces.
-- Hint: you may find the "read" function and "isDigit" function useful. 

digits :: Parser Int
digits = token $ do
  x <- many1 (sat isDigit)
  pure (read x)

-- e.g.,
-- > parse "123 abc" $ digits
-- Just (123,"abc")

-- The following parser combinator "chainl" combines a parser that returns a value x_i 
-- and a parser that returns a binary operation f, into a new parser that
-- parses for example x_1 `f` x_2 `f` x_3 `f` x4. 
-- So chainl is like sepby1, except we parse 
-- the separator as an operation, and the operation
-- is assumed to be left associated. 
chainl :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl p op = do
  a <- p
  rest a
  where rest x =
          do{ f <- op; b <- p ; rest (f x b)}
          <|> return x


-- Problem 3. (2 points). Define 'addop' that parses a '+' or a '-' symbol and then
-- return the (+) or (-) operatoin. Note: you must take care the trailing whitespaces. 
addop :: Parser (Int -> Int -> Int)
addop = token $ 
  do {
    char '-';
    pure (-)
  } 
  <|> 
  do {
    char '+';
    pure (+)
  }

-- Problem 4. (2 points). Define 'mulop' that parses a '*' or a '/' symbol and then
-- return the (*) or "div" operatoin. Note: you must take care the trailing whitespaces. 

mulop :: Parser (Int -> Int -> Int)
mulop = token $
  do {
    char '*';
    pure (*)
  } 
  <|>
  do {
    char '/';
    pure div
  }

-- An arithmetic expression can be expressed by the following grammar.
-- exp ::= term | term '+' term | term '-' term 
-- term ::= factor | factor '*' factor | factor '/' factor
-- factor ::= digits | '(' expr ')'

-- Problem 5. (6 points) Define a parser for expr. Hint: you may find `chainl` useful.  

-- Helper "factor" solves all expressions inside of parentheses
factor :: Parser Int
factor = digits <|> do
  char '('
  e <- expr
  char ')'
  return e

-- Helper "term" solves parentheses, then multiplication/division ops
term :: Parser Int
term = chainl factor mulop

-- Final "expr" solves all ops
expr :: Parser Int
expr = chainl term addop

-- e.g.,
-- > parse "1 + 3 * 4" expr
-- Just (13,"")


-- The following codes are from the class.
----------------------------------------
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (P p) =
    P $ \ s -> fmap (\ (x, s') -> (f x, s')) (p s)


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
  do {
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
  do { x <- p;
       xs <- many (sep >> p);
       return (x:xs)
     } <|> return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep =
  do { x <- p;
       xs <- many (sep >> p);
       return (x:xs)
     }
