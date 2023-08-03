module Value (Environment, Value (..)) where

import qualified Data.Map   as M
import           Expression

type Environment = M.Map Ident Expression

data Value
  = VInt !Integer
  | VFunc !Environment ![Ident] !Expression
  | VNativeFunc !([Value] -> Maybe Value)

instance Show Value where
  show (VInt n)         = show n
  show (VFunc _ args _) = "(fn (" ++ unwords args ++ ") ?)"
  show (VNativeFunc _)  = "|built-in function|"

