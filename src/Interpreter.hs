module Interpreter (Value (..), interpret) where

import           Control.Applicative ((<|>))
import qualified Data.Map            as M
import           Data.Maybe
import           Expression          (Expression (..), Ident)
import           StandardLibrary     (builtins)
import           Value

interpret :: Environment -> Expression -> Maybe Value
interpret _ (EInt n)               = Just $ VInt n
interpret env (EVar ident)         = callVar env ident
interpret env (ECall s exs)        = callFunc env s exs
interpret env (EIf ex' ex2 ex3)    = callIf env ex' ex2 ex3
interpret env (ELet pairs expr)    = callLet env pairs expr
interpret env (ELetRec pairs expr) = callLetRec env pairs expr
interpret env (EFunc ss ex')       = Just $ VFunc env ss ex'

callVar :: Environment -> Ident -> Maybe Value
callVar env ident =
  M.lookup ident env
  <|> M.lookup ident builtins

callIf :: Environment -> Expression -> Expression -> Expression -> Maybe Value
callIf env cond tb fb = do
  cond' <- interpret env cond
  case cond' of
    VFunc {} -> Nothing
    VNativeFunc {} -> Nothing
    VInt x -> if x /= 0
      then interpret env tb
      else interpret env fb

callLet :: Environment -> [(Ident, Expression)] -> Expression -> Maybe Value
callLet env pairs expr = do
  args' <- interpretBatch env exprs
  let boundEnv = M.fromList $ zip idents args'
  let newEnv = boundEnv `M.union` env
  interpret newEnv expr
  where
    (idents, exprs) = unzip pairs

-- Uhhh, so problem: I do not think it is possible to implement
-- letrec with Environment being modeled like this...
callLetRec :: Environment -> [(Ident, Expression)] -> Expression -> Maybe Value
callLetRec env pairs expr = do
  error "To be implemented"

callFunc :: Environment -> Expression -> [Expression] -> Maybe Value
callFunc env funcExpr argExrps = do
  args <- interpretBatch env argExrps
  func <- interpret env funcExpr

  case func of
    VInt _ -> Nothing
    VNativeFunc f -> f args
    VFunc closureEnv params body ->
      if isSameLength params argExrps
        then let boundEnv = M.fromList (zip params args)
              in interpret (boundEnv `M.union` closureEnv) body
        else Nothing

interpretBatch :: Environment -> [Expression] -> Maybe [Value]
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

