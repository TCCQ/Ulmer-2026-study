module Test.Interp00 (tests) where

import Ast
import Interpret
import Test.Harness

tests :: [TestCase]
tests =
  [ testCase "projection rejects invalid operands and indices" $ do
      assertLeft "project a non-tuple" $
        d2expEvaluateNil (DProj 0 (DInt 1))
      let singleton = DTuple [DInt 7]
      assertLeft "negative index" $
        d2expEvaluateNil (DProj (-1) singleton)
      assertLeft "index past the end" $
        d2expEvaluateNil (DProj 1 singleton)
      assertLeft "index into an empty tuple" $
        d2expEvaluateNil (DProj 0 (DTuple []))

  , testCase "empty tuple" $ do
      assertEqual "dValNil is the empty tuple" (DValTupl []) dValNil
      assertValue "empty tuple evaluates" (DValTupl []) $
        d2expEvaluateNil (DTuple [])

  , testCase "tuple evaluates elements and preserves nesting" $ do
      let expression =
            DTuple
              [ DOp2 (testName "+") (DVar (testName "x")) (DInt 1)
              , DTuple [DStr "nested"]
              ]
          expected =
            DValTupl
              [ DValInt 11
              , DValTupl [DValStr "nested"]
              ]
      assertValue "nested tuple" expected $
        d2expEvaluate expression [(testName "x", DValInt 10)]

  , testCase "let bindings are sequential and scoped" $ do
      let original = [(testName "x", DValInt 10)]
          expression =
            DLet
              [ DBind (testName "x")
                  (DOp2 (testName "+") (DVar (testName "x")) (DInt 1))
              , DBind (testName "y") (DVar (testName "x"))
              ]
              (Just (DVar (testName "y")))
      assertValue "body sees the rebound x" (DValInt 11) $
        d2expEvaluate expression original
      assertEqual "original environment untouched"
        (Just (DValInt 10)) (lookup (testName "x") original)

  , testCase "let returned closure retains bindings" $
      assertValue "closure over a let binding" (DValStr "captured") $
        d2expEvaluateNil
          (DApp (DLet [DBind (testName "x") (DStr "captured")]
                         (Just (DLam (testName "unused", anyType)
                                     (DVar (testName "x")))))
                (DInt 0))

  , testCase "let without body evaluates declarations" $
      assertValue "bare let is the empty tuple" (DValTupl []) $
        d2expEvaluateNil (DLet [] Nothing)

  , testCase "binding shadows after evaluating rhs" $ do
      let original = [(testName "x", DValInt 10)]
      result <- unwrap "evaluating the binding" $
        d2eclEvaluate (DBind (testName "x")
                         (DOp2 (testName "+") (DVar (testName "x")) (DInt 1)))
                      original
      assertEqual "shadowed binding" (Just (DValInt 11)) (lookup (testName "x") result)
      assertEqual "original environment untouched"
        (Just (DValInt 10)) (lookup (testName "x") original)
      assertEqual "result extends the original" original (tail result)

  , testCase "local exports preserve order and private closures" $ do
      let original = [(testName "hidden", DValInt 100)]
          private = [DBind (testName "hidden") (DInt 10)]
          public =
            [ DBind (testName "f")
                (DLam (testName "x", anyType) (DVar (testName "hidden")))
            , DBind (testName "y") (DVar (testName "hidden"))
            , DBind (testName "y")
                (DOp2 (testName "+") (DVar (testName "y")) (DInt 1))
            ]
      result <- unwrap "evaluating the local" $
        d2eclEvaluate (DLocal private public) original
      assertEqual "private binding is not exported"
        (Just (DValInt 100)) (lookup (testName "hidden") result)
      assertEqual "latest y wins" (Just (DValInt 11)) (lookup (testName "y") result)
      assertValue "closure captured the private binding" (DValInt 10) $
        d2expEvaluate (DApp (DVar (testName "f")) (DInt 0)) result
      emptyLocal <- unwrap "evaluating a local with no bindings" $
        d2eclEvaluate (DLocal private []) original
      assertEqual "local with no bindings returns the original"
        original emptyLocal
      emptyList <- unwrap "evaluating an empty declaration list" $
        d2eclistEvaluate [] original
      assertEqual "empty declaration list returns the original"
        original emptyList

  , testCase "unsupported declarations raise" $
      assertLeft "template implementation" $
        d2eclEvaluate (DImpl (TImpl (testName "f", [], DInt 1, [], 0))) []

  , testCase "string literals preserve contents" $
      mapM_ (\s -> assertValue ("literal " ++ show s) (DValStr s) $
                       d2expEvaluateNil (DStr s))
        ["", "hello", "λ\n世界"]

  , testCase "string argument passes through lambda" $
      assertValue "identity on a string" (DValStr "hello") $
        d2expEvaluateNil (DApp (DLam (testName "x", anyType) (DVar (testName "x")))
                               (DStr "hello"))

  , testCase "lambda body uses captured environment" $ do
      closure <- unwrap "building the closure" $
        d2expEvaluate (DLam (testName "x", anyType)
                            (DOp2 (testName "+")
                                  (DVar (testName "x")) (DVar (testName "y"))))
                      [(testName "y", DValInt 10)]
      let caller = [(testName "f", closure), (testName "y", DValInt 100)]
      assertValue "captured y shadows the caller y" (DValInt 17) $
        d2expEvaluate (DApp (DVar (testName "f")) (DInt 7)) caller

  , testCase "fix body can call itself" $
      assertValue "5!" (DValInt 120) $
        d2expEvaluateNil
          (DApp (DFix (testName "fact") (testName "n", anyType)
                        (DIf (DOp2 (testName "==") (DVar (testName "n")) (DInt 0))
                             (DInt 1)
                             (DOp2 (testName "*") (DVar (testName "n"))
                               (DApp (DVar (testName "fact"))
                                     (DOp2 (testName "-") (DVar (testName "n"))
                                           (DInt 1)))))
                        anyType)
                (DInt 5))
  ]
