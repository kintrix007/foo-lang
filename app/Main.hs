module Main (main) where

import           Data.Map           as M
import           ExprParse          (runExprParser)
import           Interpreter        (interpret)
import           System.Environment (getArgs)

main :: IO ()
main = do
  [filePath] <- getArgs
  sourceCode <- readFile filePath

  let ast = case runExprParser sourceCode of
        Left x  -> error x
        Right x -> x

  let res = case interpret M.empty ast of
        Left x -> error x
        Right x  -> x

  print res
