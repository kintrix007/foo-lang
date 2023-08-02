module Interpreter (Value (..), interpret) where

import qualified Data.Map   as M
import           Data.Maybe
import           Expression (Expression (..), Ident)

data Value
  = VInt !Integer
  | VFunc !Enviornment ![Ident] !Expression
  deriving (Show, Eq)

type Enviornment = M.Map Ident Value

interpret :: Enviornment -> Expression -> Maybe Value
interpret _ (EInt n)               = Just $ VInt n
interpret env (EVar s)             = M.lookup s env
interpret env (ECall s exs)        = callFunc env s exs
interpret env (EIf ex' ex2 ex3)    = callIf env ex' ex2 ex3
interpret env (ELet pairs expr)    = callLet env pairs expr
interpret env (ELetRec pairs expr) = callLetRec env pairs expr
interpret env (EFunc ss ex')       = Just $ VFunc env ss ex'

callIf :: Enviornment -> Expression -> Expression -> Expression -> Maybe Value
callIf env cond tb fb = do
  cond' <- interpret env cond
  case cond' of
    VFunc {} -> Nothing
    VInt x -> if x /= 0
      then interpret env tb
      else interpret env fb

callLet :: Enviornment -> [(Ident, Expression)] -> Expression -> Maybe Value
callLet env pairs expr = do
  args' <- interpretBatch env exprs
  let boundEnv = M.fromList $ zip idents args'
  let newEnv = boundEnv `M.union` env
  interpret newEnv expr
  where
    (idents, exprs) = unzip pairs

-- Uhhh, so problem: I do not think it is possible to implement
-- letrec with Environment being modeled like this...
callLetRec :: Enviornment -> [(Ident, Expression)] -> Expression -> Maybe Value
callLetRec env pairs expr = do
  error "To be implemented"

callFunc :: Enviornment -> Expression -> [Expression] -> Maybe Value
callFunc env funcExpr argExrps = do
  args <- interpretBatch env argExrps
  func <- interpret env funcExpr

  case func of
    VInt _ -> Nothing
    VFunc closureEnv params body ->
      if isSameLength params argExrps
        then let boundEnv = M.fromList (zip params args)
              in interpret (boundEnv `M.union` closureEnv) body
        else Nothing

interpretBatch :: Enviornment -> [Expression] -> Maybe [Value]
interpretBatch env args = do
  flattened
  where
    interpreted = map (interpret env) args
    flattened = if all isJust interpreted
      then Just $ map fromJust interpreted
      else Nothing

isSameLength :: [a] -> [b] -> Bool
isSameLength [] []         = True
isSameLength (_:xs) (_:ys) = isSameLength xs ys
isSameLength _ _           = False

