module Interpreter (Value (..), interpret) where

import           Data.Either     (isRight)
import           Data.List       (intercalate)
import qualified Data.Map        as M
import           Expression      (Expression (..), Ident)
import           StandardLibrary (builtins)
import           Value

interpret :: Environment -> Expression -> Either String Value
interpret _ (EInt n)               = Right $ VInt n
interpret env (EVar ident)         = callVar env ident
interpret env (ECall func args)    = callFunc env func args
interpret env (EIf cond tb fb)     = callIf env cond tb fb
interpret env (ELet pairs expr)    = callLet env pairs expr
interpret env (ELetRec pairs expr) = callLetRec env pairs expr
interpret env (EFunc params body)  = Right $ VFunc env params body

callVar :: Environment -> Ident -> Either String Value
callVar env ident =
  maybeToEither msg (M.lookup ident env)
  <> maybeToEither msg (M.lookup ident builtins)
  where
    msg = "Identifier '" ++ ident ++ "' does not exist."

callIf :: Environment -> Expression -> Expression -> Expression -> Either String Value
callIf env cond tb fb = do
  cond' <- interpret env cond
  case cond' of
    VFunc {} -> Left "Cannot use a function as the condition in if."
    VNativeFunc {} -> Left "Cannot use a function as the condition in if."
    VInt x -> if x /= 0
      then interpret env tb
      else interpret env fb

callLet :: Environment -> [(Ident, Expression)] -> Expression -> Either String Value
callLet env pairs body = do
  args <- interpretBatch env exprs
  let boundEnv = M.fromList $ zip idents args
  let newEnv = boundEnv `M.union` env
  interpret newEnv body
  where
    (idents, exprs) = unzip pairs

callLetRec :: Environment -> [(Ident, Expression)] -> Expression -> Either String Value
callLetRec env pairs body = do
  args <- interpretBatch env exprs
  let boundEnv = M.fromList $ zip idents args
  let newEnv = boundEnv `M.union` env
  interpret newEnv body
  where
    (idents, exprs) = unzip pairs

callFunc :: Environment -> Expression -> [Expression] -> Either String Value
callFunc env funcExpr argExrps = do
  args <- interpretBatch env argExrps
  func <- interpret env funcExpr

  case func of
    VInt _ -> Left ("Attempted to call '" ++ show func
      ++ "', but it is not a function.")
    VNativeFunc f -> f args
    VFunc closureEnv params body ->
      if isSameLength params argExrps
        then let boundEnv = M.fromList (zip params args)
              in interpret (boundEnv `M.union` closureEnv) body
        else Left ("Attempted to call '" ++ show func ++ "' with "
          ++ unwords params ++ ", but argument count does not match.")

interpretBatch :: Environment -> [Expression] -> Either String [Value]
interpretBatch env args = do
  flattened
  where
    interpreted = map (interpret env) args
    flattened = if all isRight interpreted
      then Right [x | Right x <- interpreted]
      else Left ("Errors encountered:\n" ++ intercalate "\n" lineList)
    numberLines ls = zipWith (\n l -> show n ++ ". " ++ l) [1 :: Int ..] ls
    lineList = map ("  " ++ ) $ numberLines [x | Left x <- interpreted]

isSameLength :: [a] -> [b] -> Bool
isSameLength [] []         = True
isSameLength (_:xs) (_:ys) = isSameLength xs ys
isSameLength _ _           = False

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither x Nothing  = Left x
maybeToEither _ (Just x) = Right x

