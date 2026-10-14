{- | Program-level type-checker tests, mirroring
@contrib/githwxi/FWATS3/srcgen2/tests/test_tcheck.py@.

Each program is parsed, bound and type-checked; the body's type is what a
program-level check answers with, just as @program_tcheck@ does.  The cases
srcgen2 writes with @list@\/@strm@, division, modulo, unary operators, string or
boolean equality, or the library file have no counterpart here and are recorded
in @haskell-v1/TEST-PARITY.md@.
-}
module Test.Typecheck02 (tests) where

import Control.Monad (forM_)

import Ast
import Test.Harness
import Test.Program
import Typecheck (evalTT, typecheck)

tests :: [TestCase]
tests =
  [ testCase "literals and operators" $ do
      assertSourceType "42" intType
      assertSourceType "\"hello\"" stringType
      assertSourceType "true" boolType
      assertSourceType "()" unitType
      assertSourceType "1 < 2" boolType
      assertSourceType "(lam (x: Int) => x + 1)(2)" intType

  , testCase "functions, tuples, recursion and let" $ do
      assertSourceType
        (unlines
           [ "val f = fix f(n: Int): Int -> Int =>"
           , "  if n <= 1 then 1 else n * f(n - 1);"
           , "let val pair = (f(5), true) in pair.0 end"
           ]) intType
      assertSourceType "let val x = 1 end" unitType
      assertSourceType "val x = 1; val x = x + 1; x" intType
      assertSourceType
        "(lam (f: Int -> Bool) => f(1))(lam (x: Int) => true)" boolType

  , testCase "generic and specialized implementations" $ do
      assertSourceType
        (unlines
           [ "dec id[a]: a -> a;"
           , "imp{b} id[b] = lam (x: b) => x;"
           , "imp id[Int] = lam (x: Int) => x;"
           , "imp{x,y} id[(x,y)] = lam (pair: (x,y)) => pair;"
           , "@id[(Int,Bool)](1, true).1"
           ]) boolType
      assertSourceType "dec answer: Int; imp answer = 42; @answer" intType
      assertSourceType "dec answer: Int; @answer[]" intType
      assertSourceType "dec f[a]: a -> a; @f[String](\"ok\")" stringType

  , testCase "local exports and captures" $ do
      assertSourceType
        (unlines
           [ "local"
           , "  val hidden = 1;"
           , "  dec helper: Int;"
           , "  imp helper = hidden"
           , "in"
           , "  val exposed = lam (u: ()) => @helper;"
           , "  dec public: () -> Int;"
           , "  imp public = exposed"
           , "end;"
           , "@public()"
           ]) intType
      assertSourceType
        "val x = 1; local val x = true in val y = x end; (x, y).1" boolType
      assertSourceLeft "local val hidden = 1 in val ok = 2 end; hidden"
      assertSourceLeft
        "local dec helper: Int; imp helper = 1 in val ok = 2 end; @helper"

  , testCase "nested static binders do not capture" $ do
      assertSourceType
        (unlines
           [ "dec outer[a]: a -> a;"
           , "imp{a} outer[a] = lam (x: a) =>"
           , "  let"
           , "    dec inner[a]: a -> a;"
           , "    imp{a} inner[a] = lam (y: a) => y"
           , "  in @inner[a](x) end;"
           , "@outer[Int](1)"
           ]) intType
      assertSourceLeft
        (unlines
           [ "dec outer[a]: a -> a;"
           , "imp{a} outer[a] = lam (x: a) =>"
           , "  let"
           , "    dec inner[a]: a -> a;"
           , "    imp{a} inner[a] = lam (y: a) => x"
           , "  in x end;"
           , "0"
           ])

  , testCase "expression errors" $
      forM_ [ "x"
            , "@unknown"
            , "1(true)"
            , "(lam (x: Int) => x)(false)"
            , "if 1 then 2 else 3"
            , "if true then 1 else false"
            , "(1,).1"
            , "1.0"
            , "true + 1"
            , "1 % false"
            , "(lam (x: Int) => x) == (lam (x: Int) => x)"
            , "fix f(x: Int): Bool -> Bool => x"
            , "let val x = x in x end"
            , "val f = lam (x: Int) => x; f[Int](1)"
            , "let val x = 1 in x end; x"
            , "lam (x: list) => x"
            ] $ \source ->
        assertSourceLeft source

  , testCase "declaration errors" $
      forM_ [ "imp missing = 1;\n0"
            , "dec f[a]: a -> a; imp f[Int] = lam (x: Bool) => x;\n0"
            , "dec f[a]: a -> a; imp{a} f[a] = lam (x: a) => 1;\n0"
            , "dec f[a]: a -> a; imp{a} f[a] = lam (x: a) => x + 1;\n0"
            , "dec f[a]: a -> a; imp f = lam (x: Int) => x;\n0"
            , "dec f[a]: a -> a; imp f[Int,Bool] = 1;\n0"
            , "dec f: list[Int,Bool];\n0"
            ] $ \source ->
        assertSourceLeft source

  , testCase "type application errors" $
      -- srcgen2 also rejects @f, @f[] and @f[Int,Bool] against a declaration
      -- dec f[a]: a -> a, but this checker does not enforce arity.
      assertSourceLeft "dec f[a]: a -> a; @f[Int](false)"

  , testCase "direct syntax-tree validation" $ do
      -- An operator that the checker does not know is rejected.
      let unknown = DOp2 (testName "unknown") (DInt 1) (DInt 2)
      case evalTT (typecheck unknown) 0 of
        Left _ -> pure ()
        Right _ -> ioError . userError $
          "an unknown binary operator was not rejected"
  ]
