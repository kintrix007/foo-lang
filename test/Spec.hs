import qualified ParsingTest as PT
import qualified InterpreterTest as IT
import           Test.HUnit

main :: IO ()
main = runTestTTAndExit $ TestList
  [ PT.tests
  , IT.tests
  ]
