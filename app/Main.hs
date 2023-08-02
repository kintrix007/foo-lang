module Main (main) where

import           Data.Map           as M
import           ExprParse          (runExprParser)
import           System.Environment (getArgs)
import Interpreter (interpret, Value (..))

main :: IO ()
main = do
  [filePath] <- getArgs
  sourceCode <- readFile filePath

  let ast = case runExprParser sourceCode of
        Left x  -> error x
        Right x -> x

  let res = case interpret M.empty ast of
        Nothing -> error "Runtime error"
        Just x -> x
  
  case res of
    VInt x -> print x
    VFunc _ x _ -> putStrLn ("(fn (" ++ unwords x ++ ") ?)")
