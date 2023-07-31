{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}
module Parser
( Parser (..)
, parse
, item
, sat
, char
, string
, nat
, int
, space
, token
, integer
, ident
) where

import           Control.Applicative
import           Control.Monad
import           Data.Char           (isDigit, isSpace)

newtype Parser c a = Parser ([c] -> Maybe (a, [c]))

parse :: Parser c a -> [c] -> Maybe (a, [c])
parse (Parser p) inp = p inp

instance Functor (Parser c) where
  fmap :: (a -> b) -> Parser c a -> Parser c b
  fmap fab (Parser p) = Parser $ \inp ->
    case p inp of
      Nothing        -> Nothing
      Just (a, inp') -> Just (fab a, inp')

instance Applicative (Parser c) where
  pure :: a -> Parser c a
  pure a = Parser (\inp -> Just (a, inp))
  (<*>) :: Parser c (a -> b) -> Parser c a -> Parser c b
  (Parser f) <*> pa = Parser $ \inp ->
    case f inp of
      Nothing          -> Nothing
      Just (fab, inp') -> parse (fab <$> pa) inp'

instance Monad (Parser c) where
  (>>=) :: Parser c a -> (a -> Parser c b) -> Parser c b
  (Parser g) >>= f = Parser $ \inp ->
    case g inp of
      Nothing        -> Nothing
      Just (a, inp') -> parse (f a) inp'

instance Alternative (Parser c) where
  empty :: Parser c a
  empty = Parser (\_ -> Nothing)
  (<|>) :: Parser c a -> Parser c a -> Parser c a
  (Parser f) <|> (Parser g) = Parser $ \inp ->
    case f inp of
      Nothing -> g inp
      Just x  -> Just x

instance MonadFail (Parser c) where
  fail :: String -> Parser c a
  fail = const empty

item :: Parser c c
item = Parser $ \case
  []       -> Nothing
  (c:inp') -> Just (c, inp')

sat :: (c -> Bool) -> Parser c c
sat p = do
  c <- item
  if p c
    then return c
    else empty

char :: Char -> Parser Char Char
char ch = sat (==ch)

string :: String -> Parser Char String
string "" = return ""
string (c:cs) = do
  _ <- char c
  _ <- string cs
  return (c:cs)

nat :: Parser Char String
nat = some (sat isDigit)

int :: Parser Char String
int =
  do
    s <- char '-'
    n <- nat
    return (s:n)
  <|> nat

integer :: Parser Char Integer
integer = read <$> int

space :: Parser Char ()
space = void $ many (sat isSpace)

ident :: Parser Char String
ident =
  some (sat (not . isSpace))
  <|> -- (char '|' >> some (sat (/='|')) >>= \i -> char '|' >> return i)
    do
      _ <- char '|'
      i <- some $ sat (/= '|')
      _ <- char '|'
      return i

token :: Parser Char a -> Parser Char a
token p = do
  space
  x <- p
  space
  return x

