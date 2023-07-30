module ExprParse (parseExpression) where

import           Control.Applicative
import           Expression
import           Parser

parseInt :: Parser Char Expression
parseInt = EInt <$> token integer

parseIf :: Parser Char Expression
parseIf = do
  _ <- token $ string "if"
  cond <- parseExpression
  trueCase <- parseExpression
  falseCase <- parseExpression
  return $ EIf cond trueCase falseCase

parseVar :: Parser Char Expression
parseVar = EVar <$> token ident

parseLetPairs :: Parser Char [(String, Expression)]
parseLetPairs = do
  _ <- token $ char '('

  pairs <- some $ do
    _ <- char '('
    i    <- token ident
    expr <- token parseExpression
    _ <- char ')'
    return (i, expr)

  _ <- token $ char ')'
  return pairs

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
  _ <- char '('
  params <- some $ token ident
  _ <- char ')'
  space
  _ <- char '('
  body <- parseExpression
  _ <- char ')'
  return $ EFunc params body

parseCall :: Parser Char Expression
parseCall = do
  i <- token ident
  args <- some $ token parseExpression
  return $ ECall i args

parseExpression :: Parser Char Expression
parseExpression =
  do
    _ <- token (char '(')
    x <- parsers
    _ <- token (char ')')
    return x
  <|> token parsers
  where
    parsers = parseInt <|> parseIf
      <|> parseLet <|> parseLetRec
      <|> parseFunc <|> parseCall <|> parseVar
