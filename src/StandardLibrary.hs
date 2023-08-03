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

addition :: [Value] -> Either String Value
addition [VInt x, VInt y] = return $ VInt (x + y)
addition args             = Left $ wrongArgs args "(int int)"

subtraction :: [Value] -> Either String Value
subtraction [VInt x]         = return $ VInt (-x)
subtraction [VInt x, VInt y] = return $ VInt (x - y)
subtraction args             = Left $ wrongArgs args "(int) or (int int)"

multiplication :: [Value] -> Either String Value
multiplication [VInt x, VInt y] = return $ VInt (x * y)
multiplication args             = Left $ wrongArgs args "(int int)"

division :: [Value] -> Either String Value
division [VInt x, VInt y] = return $ VInt (x `div` y)
division args             = Left $ wrongArgs args "(int int)"

print' :: [Value] -> Either String Value
print' [x]  = return $ trace (show x) x
print' args = Left $ wrongArgs args "(int) or (fn)"

debug' :: [Value] -> Either String Value
debug' xs = return $ trace (unwords $ map show xs) (last xs)

wrongArgs :: [Value] -> String -> String
wrongArgs args expected = "Wrong argument types. Expected " ++ expected
  ++ ", however received (" ++ concatMap typeof args ++ ")."

typeof :: Value -> String
typeof VInt {}        = "int"
typeof VFunc {}       = "fn"
typeof VNativeFunc {} = "fn"
