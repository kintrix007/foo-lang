module Expression (Expression (..), Ident) where

type Ident = String

data Expression
  = EInt !Integer
  | EVar !Ident
  | ECall !Expression ![Expression]
  | EIf !Expression !Expression !Expression
  | ELet ![(Ident, Expression)] !Expression
  | ELetRec ![(Ident, Expression)] !Expression
  | EFunc ![Ident] !Expression
  deriving (Show, Eq)
