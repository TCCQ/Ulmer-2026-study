{- | Program-level interpreter tests, mirroring
@contrib/githwxi/FWATS3/srcgen2/tests/test_interp.py@.

Each program is run exactly as srcgen2's @run@ helper does: parse, type-check,
expand templates, then evaluate.  The cases srcgen2 writes with custom type
constructors or a library file have no counterpart in this language and are
recorded in @haskell-v1/TEST-PARITY.md@.
-}
module Test.Interp02 (tests) where

import Ast
import Interpret
import Test.Harness
import Test.Program

tests :: [TestCase]
tests =
  [ testCase "primitive values and control flow" $ do
      assertRun "()" (DValTupl [])
      assertRun "(42,)" (DValTupl [DValInt 42])
      assertRun "(1,true,\"ok\").2" (DValStr "ok")
      assertRun "2*3+1" (DValInt 7)
      assertRun "-7 / 3" (DValInt (-3))
      assertRun "-7 % 3" (DValInt 2)
      assertRun "+2*3+1" (DValInt 7)
      assertRun "\"a\" != \"b\"" (DValBtf True)
      assertRun "false == false" (DValBtf True)
      assertRun "if true then 42 else 43" (DValInt 42)
      assertRun "if false then 1 else 42" (DValInt 42)
      assertRun "let val x = 1 end" (DValTupl [])
      assertRun "val x = 1; val x = x + 2; x" (DValInt 3)

  , testCase "if evaluates only the branch it takes" $ do
      assertRun "if true then 42 else 1/0" (DValInt 42)
      assertRun "if false then 1/0 else 42" (DValInt 42)

  , testCase "a lambda captures both environments" $
      assertRun
        (unlines
           [ "dec value: Int;"
           , "imp value = 10;"
           , "val x = 1;"
           , "val f = lam (u: ()) => x + @value;"
           , "val x = 100;"
           , "imp value = 20;"
           , "(f(), @value, x)"
           ]) (DValTupl [DValInt 11, DValInt 20, DValInt 100])

  , testCase "a fix captures both environments" $
      assertRun
        (unlines
           [ "dec step: Int;"
           , "imp step = 2;"
           , "val initial = 10;"
           , "val f = fix loop(n: Int): Int -> Int =>"
           , "  if n == 0 then initial else @step + loop(n - 1);"
           , "val initial = 100;"
           , "imp step = 20;"
           , "f(3)"
           ]) (DValInt 16)

  , testCase "an implementation captures both environments" $
      assertRun
        (unlines
           [ "dec helper: Int;"
           , "imp helper = 10;"
           , "val x = 1;"
           , "dec f[a]: a -> Int;"
           , "imp{a} f[a] = lam (ignored: a) => x + @helper;"
           , "val x = 100;"
           , "imp helper = 20;"
           , "@f[Bool](true)"
           ]) (DValInt 11)

  , testCase "repeated patterns and latest-matching selection" $ do
      assertRun
        (unlines
           [ "dec choose[a,b]: () -> Int;"
           , "imp{x,y} choose[x,y] = lam (u: ()) => 1;"
           , "imp{x} choose[x,x] = lam (u: ()) => 2;"
           , "(@choose[Int,Bool](), @choose[Int,Int]())"
           ]) (DValTupl [DValInt 1, DValInt 2])
      assertRun
        (unlines
           [ "dec choose[a]: () -> Int;"
           , "imp{x} choose[x] = lam (u: ()) => 1;"
           , "imp choose[Int] = lam (u: ()) => 2;"
           , "(@choose[Int](), @choose[Bool]())"
           ]) (DValTupl [DValInt 2, DValInt 1])
      -- The newest matching implementation wins, not the most specific one.
      assertRun
        (unlines
           [ "dec choose[a]: () -> Int;"
           , "imp choose[Int] = lam (u: ()) => 2;"
           , "imp{x} choose[x] = lam (u: ()) => 1;"
           , "@choose[Int]()"
           ]) (DValInt 1)

  , testCase "compound patterns and static substitution" $ do
      assertRun
        (unlines
           [ "dec id[t]: t -> t;"
           , "imp{a,b} id[(a,b)] = lam (pair: (a,b)) => pair;"
           , "@id[(Int,Bool)](7, true)"
           ]) (DValTupl [DValInt 7, DValBtf True])
      assertRun
        (unlines
           [ "dec choose[a]: () -> Int;"
           , "imp choose[Int] = lam (u: ()) => 7;"
           , "imp choose[Bool] = lam (u: ()) => 9;"
           , "dec outer[a]: () -> Int;"
           , "imp{a} outer[a] ="
           , "  let"
           , "    dec inner[b]: () -> Int;"
           , "    imp{b} inner[b] = lam (u: ()) => @choose[a]();"
           , "    dec shadow[a]: () -> Int;"
           , "    imp{a} shadow[a] = lam (u: ()) => @choose[a]()"
           , "  in lam (u: ()) => @inner[Bool]() + @shadow[Bool]() end;"
           , "@outer[Int]()"
           ]) (DValInt 16)

  , testCase "local exports capture private environments" $ do
      assertRun
        (unlines
           [ "dec helper: Int;"
           , "imp helper = 1;"
           , "local"
           , "  val hidden = 40;"
           , "  imp helper = 2"
           , "in"
           , "  val f = lam (u: ()) => hidden + @helper;"
           , "  dec public: () -> Int;"
           , "  imp public = f"
           , "end;"
           , "(@public(), @helper)"
           ]) (DValTupl [DValInt 42, DValInt 1])
      -- A closure exported from a local keeps the private template.
      assertRun
        (unlines
           [ "dec private: Int;"
           , "local val secret = 1; imp private = 2"
           , "in val public = lam (u: ()) => secret + @private end;"
           , "public()"
           ]) (DValInt 3)

  , testCase "zero-argument templates and an alias" $ do
      assertRun "dec x: Int; imp x = 42; (@x, @x[])"
        (DValTupl [DValInt 42, DValInt 42])
      assertRun
        (unlines
           [ "dec x[a]: () -> Int;"
           , "imp x[Int] = lam (u: ()) => 42;"
           , "@x[Int]()"
           ]) (DValInt 42)

  , testCase "runtime errors without type-checking" $ do
      assertLeft "an unknown variable" $
        d2expEvaluateNil (DVar (testName "unknown"))
      assertLeft "an unknown template use" $
        d2expEvaluateNil (DTapp (testName "missing") [])
      assertLeft "applying a non-function" $
        d2expEvaluateNil (DApp (DInt 1) (DInt 2))
      assertLeft "an if on a non-boolean" $
        d2expEvaluateNil (DIf (DInt 1) (DInt 2) (DInt 3))
      assertLeft "adding a boolean" $
        d2expEvaluateNil (DOp2 (testName "+") (DBool True) (DInt 1))
      assertLeft "negating a boolean" $
        d2expEvaluateNil (DOp1 (testName "-") (DBool False))
      assertLeft "comparing a boolean with an integer" $
        d2expEvaluateNil (DOp2 (testName "==") (DBool True) (DInt 1))
      assertLeft "comparing two tuples" $
        d2expEvaluateNil
          (DOp2 (testName "==") (DTuple [DInt 1]) (DTuple [DInt 1]))
      assertLeft "dividing by zero" $
        d2expEvaluateNil (DOp2 (testName "/") (DInt 1) (DInt 0))
      assertLeft "taking a modulo by zero" $
        d2expEvaluateNil (DOp2 (testName "%") (DInt 1) (DInt 0))
      assertLeft "projecting a number literal" $
        d2expEvaluateNil (DProj 0 (DInt 1))
      assertLeft "projecting past the end of a tuple" $
        d2expEvaluateNil (DProj 1 (DTuple [DInt 1]))
      assertLeft "a static application cannot be interpreted" $
        d2expEvaluateNil (DTapp (testName "f") [])
  ]
