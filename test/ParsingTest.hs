module ParsingTest (tests) where

import           Data.Either (isLeft)
import           ExprParse   (runExprParser)
import           Expression  (Expression (..))
import           Test.HUnit

tests :: Test
tests = TestList
  [ tc "comment1" $ assertBool "holds" ("5 # Comment" `parseTo` EInt 5)
  , tc "comment2" $ assertBool "holds" ("5#Comment" `parseTo` EInt 5)
  , tc "comment3" $ assertBool "fails" (fails "# (+ 5 2)")
  , tc "comment4" $ assertBool "holds" ("(+ 5 # five\n 3)"
    `parseTo` ECall (EVar "+") [EInt 5, EInt 3])
  , tc "comment5" $ assertBool "fails" (fails "(+ 5 # five 3)")
  , tc "int1" $ assertBool "holds" ("42" `parseTo` EInt 42)
  , tc "int2" $ assertBool "holds" ("(6)" `parseTo` EInt 6)
  , tc "int3" $ assertBool "holds" ("(-2)" `parseTo` EInt (-2))
  , tc "int4" $ assertBool "holds" ("-1024" `parseTo` EInt (-1024))
  , tc "notInt" $ assertBool "holds" ("(- 2)" `parseTo` ECall (EVar "-") [EInt 2])
  , tc "var1" $ assertBool "holds" ("x" `parseTo` EVar "x")
  , tc "var2" $ assertBool "holds" ("<=>" `parseTo` EVar "<=>")
  , tc "var3" $ assertBool "holds" ("x+1=2" `parseTo` EVar "x+1=2")
  , tc "var4" $ assertBool "holds" ("|This has spaces|"
    `parseTo` EVar "This has spaces")
  , tc "var5" $ assertBool "holds" ("(foo|bar)" `parseTo` EVar "foo|bar")
  , tc "call1" $ assertBool "holds" ("(f x y)"
    `parseTo` ECall (EVar "f") [EVar "x", EVar "y"])
  , tc "call2" $ assertBool "holds" ("(foo (g x) (h 1 y))"
    `parseTo` ECall (EVar "foo") [ECall (EVar "g") [EVar "x"], ECall (EVar "h") [EInt 1, EVar "y"]])
  , tc "call2" $ assertBool "holds" ("(((foo) 5) 4)"
    `parseTo` ECall (ECall (EVar "foo") [EInt 5]) [EInt 4])
  , tc "notVar" $ assertBool "fails" (fails "1+1=2")
  , tc "if1" $ assertBool "holds" ("(if 1 2 3)"
    `parseTo` EIf (EInt 1) (EInt 2) (EInt 3))
  , tc "if2" $ assertBool "holds" ("(if (> 42 0) (1) (0))"
    `parseTo` EIf (ECall (EVar ">") [EInt 42, EInt 0]) (EInt 1) (EInt 0))
  , tc "ifFails1" $ assertBool "fails" (fails "(if (= x 1))")
  , tc "ifFails2" $ assertBool "fails" (fails "(if (= x 1) (print 42))")
  , tc "let1" $ assertBool "holds" ("(let ((a 5))(a))"
    `parseTo` ELet [("a", EInt 5)] (EVar "a"))
  , tc "let2" $ assertBool "holds" ("(let ((a 5)) a)"
    `parseTo` ELet [("a", EInt 5)] (EVar "a"))
  , tc "let3" $ assertBool "holds" ("(let ((x 7) (y 42))(x))"
    `parseTo` ELet [("x", EInt 7), ("y", EInt 42)] (EVar "x"))
  , tc "let4" $ assertBool "holds" ("(let ((x 1))(let ((y x))(y)))"
    `parseTo` ELet [("x", EInt 1)] (ELet [("y", EVar "x")] (EVar "y")))
  , tc "letFails1" $ assertBool "fails" (fails "(let () (5))")
  , tc "letrec1" $ assertBool "holds" ("(letrec ((a 5))(a))"
    `parseTo` ELetRec [("a", EInt 5)] (EVar "a"))
  , tc "letrec2" $ assertBool "holds" ("(letrec ((a 5)) a)"
    `parseTo` ELetRec [("a", EInt 5)] (EVar "a"))
  , tc "letrec3" $ assertBool "holds" ("(letrec ((x 7) (y 42))(x))"
    `parseTo` ELetRec [("x", EInt 7), ("y", EInt 42)] (EVar "x"))
  , tc "letrec4" $ assertBool "holds" ("(letrec ((x 1))(letrec ((y x))(y)))"
    `parseTo` ELetRec [("x", EInt 1)] (ELetRec [("y", EVar "x")] (EVar "y")))
  , tc "letrecFails1" $ assertBool "fails" (fails "(letrec () (5))")
  , tc "func1" $ assertBool "holds" ("(fn (x y) (+ x y))"
    `parseTo` EFunc ["x", "y"] (ECall (EVar "+") [EVar "x", EVar "y"]))
  , tc "func2" $ assertBool "holds" ("(fn (|foo bar|) (* |foo bar| 2))"
    `parseTo` EFunc ["foo bar"] (ECall (EVar "*") [EVar "foo bar", EInt 2]))
  , tc "func3" $ assertBool "holds" ("(fn (x) x)"
    `parseTo` EFunc ["x"] (EVar "x"))
  , tc "funcFails1" $ assertBool "holds" (fails "(fn () (10))")
  , tc "funcFails2" $ assertBool "holds" (fails "(fn (x y)")
  , tc "funcFails3" $ assertBool "holds" (fails "(fn (10))")
  , tc "whitespace1" $ assertBool "holds"
    ("let((x y)(a b))(x(a))"
    `parseTo` ELet [("x", EVar "y"), ("a", EVar "b")] (ECall (EVar "x") [EVar "a"]))
  , tc "whitespace2" $ assertBool "holds"
    (" ( let  (  ( x  y   ) (  a b ) )   ( ( x )a ) ) "
    `parseTo` ELet [("x", EVar "y"), ("a", EVar "b")] (ECall (EVar "x") [EVar "a"]))
  , tc "whitespace3" $ assertBool "holds"
    ("(fn(x)(if(x)1 2))"
    `parseTo` EFunc ["x"] (EIf (EVar "x") (EInt 1) (EInt 2)))
  , tc "whitespace4" $ assertBool "holds"
    ("  (   fn ( x ) ( if  x  ( 1 ) ( 2 )  )  ) "
    `parseTo` EFunc ["x"] (EIf (EVar "x") (EInt 1) (EInt 2)))
  ]

tc :: String -> Assertion -> Test
tc label assertion = TestLabel label $ TestCase assertion

infixl 4 `parseTo`
parseTo :: String -> Expression -> Bool
parseTo inp expr = runExprParser inp == Right expr

fails :: String -> Bool
fails inp = isLeft $ runExprParser inp
