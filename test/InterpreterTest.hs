module InterpreterTest (tests) where
import qualified Data.Map    as M
import           ExprParse   (runExprParser)
import           Interpreter (Value (..), interpret)
import           Test.HUnit
import Expression (Expression(..))

tests :: Test
tests = TestList
  [ tc "int1" $ assertBool "holds" ("-5" `evalTo` VInt (-5))
  , tc "int2" $ assertBool "holds" ("42" `evalTo` VInt 42)
  , tc "int3" $ assertBool "holds" ("(- 69)" `evalTo` VInt (-69))
  , tc "int4" $ assertBool "holds" ("(- -69)" `evalTo` VInt 69)
  , tc "varFails1" $ assertBool "fails" (fails "x")
  , tc "varFails2" $ assertBool "fails" (fails "(x)")
  , tc "varFails3" $ assertBool "fails" (fails "(+ foo 5)")
  , tc "varFails4" $ assertBool "fails" (fails "(+ 5 bar)")
  , tc "let1" $ assertBool "holds" ("(let ((foo 7)) (42))" `evalTo` VInt 42)
  , tc "let2" $ assertBool "holds" ("(let ((foo 7)) (foo))" `evalTo` VInt 7)
  , tc "let3" $ assertBool "holds" ("(let ((foo 7) (bar 5)) (foo))"
    `evalTo` VInt 7)
  , tc "let4" $ assertBool "holds" ("(let ((foo 7) (bar 5)) (bar))"
    `evalTo` VInt 5)
  , tc "letFn1" $ assertBool "holds" ("(let ((foo (fn (x) (x)))) (foo 6))"
    `evalTo` VInt 6)
  , tc "letFnFails1" $ assertBool "holds"
    (fails "(let ((foo (fn (x) (foo x)))) (1))")
  , tc "letFnFails2" $ assertBool "holds"
    (fails "(let ((foo (fn (= x 1) (if x (* 2 (foo (- x 1))) (1))))) (1))")
  , tc "letFails1" $ assertBool "fails" (fails "(let ((foo foo)) (foo))")
  , tc "letFails2" $ assertBool "fails" (fails "(let ((foo foo)) (-1))")
  , tc "letrec1" $ assertBool "holds" ("(letrec ((foo 7)) (42))" `evalTo` VInt 42)
  , tc "letrec2" $ assertBool "holds" ("(letrec ((foo 7)) (foo))" `evalTo` VInt 7)
  , tc "letrec3" $ assertBool "holds" ("(letrec ((foo 7) (bar 5)) (foo))"
    `evalTo` VInt 7)
  , tc "letrec4" $ assertBool "holds" ("(letrec ((foo 7) (bar 5)) (bar))"
    `evalTo` VInt 5)
  , tc "letrecFails1" $ assertBool "fails" (fails "(letrec ((foo foo)) (foo))")
  , tc "letrec5" $ assertBool "fails" ("(letrec ((foo foo)) (-1))"
    `evalTo` VInt (-1))
  , tc "letrecFn1" $ assertBool "holds" ("(letrec ((foo (fn (x) (x)))) (foo 6))"
    `evalTo` VInt 6)
  -- , tc "letrecFnFails1" $ assertBool "holds"
  --     (fails "(letrec ((foo (fn (x) (foo x)))) (foo 5))")
  , tc "letrecFn2" $ assertBool "holds"
      ("(letrec ((pow (fn (x) (if (x) (* 2 (pow (- x 1))) (1))))) (pow 4))"
      `evalTo` VInt 16)
  , tc "if1" $ assertBool "holds" ("(if 1 42 69)" `evalTo` VInt 42)
  , tc "if2" $ assertBool "holds" ("(if 0 42 69)" `evalTo` VInt 69)
  , tc "if3" $ assertBool "holds" ("(if -1 42 69)" `evalTo` VInt 42)
  , tc "func1" $ assertBool "holds" ("(fn (x) (x))"
    `evalTo` VFunc M.empty ["x"] (EVar "x"))
  , tc "func2" $ assertBool "holds" ("(fn (x y z) (5))"
    `evalTo` VFunc M.empty ["x", "y", "z"] (EInt 5))
  , tc "func3" $ assertBool "holds" ("(fn (foo) (fn (bar) (foo)))"
    `evalTo` VFunc M.empty ["foo"] (EFunc ["bar"] (EVar "foo")))
  , tc "funcFails1" $ assertBool "fails" (fails "(fn (x) (bar))")
  , tc "funcFails2" $ assertBool "fails" (fails "(fn (x) (fn (y) (foo)))")
  , tc "funcFails3" $ assertBool "fails" (fails "((fn (x) (if x 10 y)) (1))")
  , tc "funcFails4" $ assertBool "fails" (fails "((fn (x) (if x 10 y)) (0))")
  ]

tc :: String -> Assertion -> Test
tc label assertion = TestLabel label $ TestCase assertion

infixl 4 `evalTo`
evalTo :: String -> Value -> Bool
evalTo inp expected =
  case runExprParser inp of
    Left _ -> False
    Right expr -> case interpret M.empty expr of
      Left _    -> False
      Right res -> res == expected

fails :: String -> Bool
fails inp =
  case runExprParser inp of
    Left _ -> False
    Right expr -> case interpret M.empty expr of
      Right _ -> False
      Left _  -> True

