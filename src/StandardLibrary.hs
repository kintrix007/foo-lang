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
  , ("<", VNativeFunc lessThan)
  , (">", VNativeFunc greaterThan)
  , ("=", VNativeFunc equals)
  , ("&", VNativeFunc booleanAnd)
  , ("|", VNativeFunc booleanOr)
  , ("~", VNativeFunc booleanNot)
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

lessThan :: [Value] -> Either String Value
lessThan [VInt x, VInt y] = return $ VInt (if x < y then 1 else 0)
lessThan args             = Left $ wrongArgs args "(int int)"

greaterThan :: [Value] -> Either String Value
greaterThan [VInt x, VInt y] = return $ VInt (if x > y then 1 else 0)
greaterThan args             = Left $ wrongArgs args "(int int)"

equals :: [Value] -> Either String Value
equals [VInt x, VInt y] = return $ VInt (if x == y then 1 else 0)
equals args             = Left $ wrongArgs args "(int int)"

booleanAnd :: [Value] -> Either String Value
booleanAnd [VInt x, VInt y] = return $ VInt (if x /= 0 && y /= 0 then 1 else 0)
booleanAnd args             = Left $ wrongArgs args "(int int)"

booleanOr :: [Value] -> Either String Value
booleanOr [VInt x, VInt y] = return $ VInt (if x /= 0 || y /= 0 then 1 else 0)
booleanOr args             = Left $ wrongArgs args "(int int)"

booleanNot :: [Value] -> Either String Value
booleanNot [VInt x] = return $ VInt (if x /= 0 then 0 else 1)
booleanNot args             = Left $ wrongArgs args "(int int)"

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
