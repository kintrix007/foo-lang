module Value (Environment, Value (..)) where

import qualified Data.Map   as M
import           Expression

data Value
  = VInt !Integer
  | VFunc !Environment ![Ident] !Expression
  | VNativeFunc !([Value] -> Either String Value)

instance Show Value where
  show (VInt n)         = show n
  show (VFunc _ args _) = "(fn (" ++ unwords args ++ ") ?)"
  show (VNativeFunc _)  = "|built-in function|"

instance Eq Value where
  (VInt n) == (VInt m) = n == m
  (VFunc env params body) == (VFunc env' params' body') =
    env == env' && params == params' && body == body'
  (VNativeFunc _) == (VNativeFunc _) = error "Cannot compare built-in function types"
  _ == _ = False

type Environment = M.Map Ident Value

