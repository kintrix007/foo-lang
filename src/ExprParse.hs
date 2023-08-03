module ExprParse (parseExpression, runExprParser, keywords) where

import           Control.Applicative
import           Data.Char
import           Expression
import           Parser

keywords :: [Ident]
keywords = ["if", "let", "letrec", "fn"]

runExprParser :: String -> Either String Expression
runExprParser inp =
  case parse parseExpression inp of
    Nothing           -> Left "Failed to parse."
    Just (expr, rest) ->
      if not $ null rest
        then Left ("Stopped parsing with leftover: '" ++ show rest ++ "'")
        else Right expr

parseComment :: Parser Char a -> Parser Char a
parseComment p =
  do
    _ <- some $ do
      space
      _ <- char '#'
      _ <- many $ sat (/= '\n')
      _ <- char '\n'
      return ()
    p
  <|> p


parseInt :: Parser Char Expression
parseInt = EInt <$> token integer

parseIf :: Parser Char Expression
parseIf = do
  _ <- token $ string "if"
  cond <- parseExpression
  trueCase <- parseExpression
  falseCase <- parseExpression
  return $ EIf cond trueCase falseCase

parseIdent :: Parser Char String
parseIdent =
  do
    _ <- char '|'
    i <- some $ sat (/= '|')
    _ <- char '|'
    return i
  <|>
  do
    i <- some $ sat (\c -> not . any ($c) $ disallowed)
    if i `elem` keywords
      then empty
      else return i
    where
      disallowed = [isSpace, (`elem` ['(', ')'])]

parseVar :: Parser Char Expression
parseVar = EVar <$> token parseIdent

parseLetPairs :: Parser Char [(String, Expression)]
parseLetPairs = do
  paren . some $ do
    _ <- token $ char '('
    i    <- token parseIdent
    expr <- token parseExpression
    _ <- token $ char ')'
    return (i, expr)

parseLet :: Parser Char Expression
parseLet = do
  _ <- token $ string "let"
  pairs <- parseLetPairs
  expr <- parseExpression
  return $ ELet pairs expr

parseLetRec :: Parser Char Expression
parseLetRec = do
  _ <- token $ string "letrec"
  pairs <- parseLetPairs
  expr <- parseExpression
  return $ ELetRec pairs expr

parseFunc :: Parser Char Expression
parseFunc = do
  _ <- token $ string "fn"

  params <- paren $ some $ token parseIdent
  space
  body <- parseExpression
  return $ EFunc params body

parseCall :: Parser Char Expression
parseCall = do
  _ <- token $ char '('
  i <- parseExpression
  args <- some $ token parseExpression
  _ <- token $ char ')'
  return $ ECall i args

parseExpression :: Parser Char Expression
parseExpression =
  paren parsers <|> token parsers
  where
    parsers = parseComment (parseInt <|> parseIf
      <|> parseLet <|> parseLetRec
      <|> parseFunc <|> parseCall <|> parseVar)
