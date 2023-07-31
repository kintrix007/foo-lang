import qualified ParsingTest as PT
import           Test.HUnit

main :: IO ()
main = runTestTTAndExit $ TestList
  [ PT.tests
  ]
