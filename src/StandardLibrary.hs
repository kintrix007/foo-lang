module StandardLibrary (builtins) where

import qualified Data.Map    as M
import           Debug.Trace (trace)
import           Value       (Environment, Value (..))

builtins :: Environment
builtins = M.fromList
  [ ("+", VNativeFunc addition)
  , ("-", VNativeFunc subtraction)
  , ("*", VNativeFunc multiplication)
  , ("/", VNativeFunc division)
  , ("print", VNativeFunc print')
  , ("debug", VNativeFunc debug')
  ]

addition :: [Value] -> Maybe Value
addition [VInt x, VInt y] = Just $ VInt (x + y)
addition _                = Nothing

subtraction :: [Value] -> Maybe Value
subtraction [VInt x]         = Just $ VInt (-x)
subtraction [VInt x, VInt y] = Just $ VInt (x - y)
subtraction _                = Nothing

multiplication :: [Value] -> Maybe Value
multiplication [VInt x, VInt y] = Just $ VInt (x * y)
multiplication _                = Nothing

division :: [Value] -> Maybe Value
division [VInt x, VInt y] = Just $ VInt (x `div` y)
division _                = Nothing

print' :: [Value] -> Maybe Value
print' [x] = Just $ trace (show x) x
print' _   = Nothing

debug' :: [Value] -> Maybe Value
debug' xs = Just $ trace (unwords $ map show xs) (last xs)
