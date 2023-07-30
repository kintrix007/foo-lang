module Parse (Expression (..)) where

type Ident = String

-- Not 100% sure if debug can be implemented
-- as a function..? Hopefully yes.
data Expression
  = EInt !Integer
  | EIf !Expression !Expression !Expression
  | EVar !Ident
  | ELet ![(Ident, Expression)]
  | ELetRec ![(Ident, Expression)]
  | EFunc ![Ident] !Expression
  | ECall !Ident ![Expression]
