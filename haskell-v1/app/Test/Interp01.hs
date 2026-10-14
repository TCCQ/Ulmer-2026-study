module Test.Interp01 (tests) where

import Ast
import Interpret
import Test.Harness

factorialLoop :: DExp Name
factorialLoop =
  DFix (testName "loop") (testName "n", anyType)
    (DLam (testName "acc", anyType)
      (DIf (DOp2 (testName "==") (DVar (testName "n")) (DInt 0))
           (DVar (testName "acc"))
           (DApp (DApp (DVar (testName "loop"))
                       (DOp2 (testName "-") (DVar (testName "n")) (DInt 1)))
                 (DOp2 (testName "*") (DVar (testName "n"))
                       (DVar (testName "acc"))))))
    anyType

tuple3 :: DExp Name -> DExp Name -> DExp Name -> DExp Name
tuple3 first second third = DTuple [first, second, third]

fibonacciLoop :: DExp Name
fibonacciLoop =
  DFix (testName "fibo") (testName "state", anyType)
    (DIf (DOp2 (testName "==") n (DInt 0))
         a
         (DApp (DVar (testName "fibo"))
               (tuple3 (DOp2 (testName "-") n (DInt 1)) b
                       (DOp2 (testName "+") a b))))
    anyType
  where
    state = DVar (testName "state")
    n = DProj 0 state
    a = DProj 1 state
    b = DProj 2 state

intFactorial :: Int -> Integer
intFactorial n = product [1 .. fromIntegral n]

tests :: [TestCase]
tests =
  [ testCase "fibonacci with let bindings" $
      assertValue "fib 10" (DValInt 55) $
        d2expEvaluateNil
          (DLet [ DBind (testName "n") (DInt 10)
                , DBind (testName "fib") fibonacciLoop
                , DBind (testName "initial")
                    (tuple3 (DVar (testName "n")) (DInt 0) (DInt 1))
                ]
                (Just (DApp (DVar (testName "fib")) (DVar (testName "initial")))))

  , testCase "fibonacci" $
      mapM_ (\(n, expected) ->
               assertValue ("fib " ++ show n) (DValInt expected) $
                 d2expEvaluateNil
                   (DApp fibonacciLoop (tuple3 (DInt n) (DInt 0) (DInt 1))))
        [(0, 0), (1, 1), (2, 1), (3, 2), (10, 55), (20, 6765)]

  , testCase "factorial" $
      mapM_ (\n ->
               assertValue ("fact " ++ show n)
                 (DValInt (fromInteger (intFactorial n))) $
                 d2expEvaluateNil
                   (DApp (DApp factorialLoop (DInt n)) (DInt 1)))
        [0, 1, 2, 5, 10, 20]

  , testCase "loop preserves initial accumulator" $
      mapM_ (\n ->
               assertValue ("loop " ++ show n ++ " with acc 3")
                 (DValInt (fromInteger (3 * intFactorial n))) $
                 d2expEvaluateNil
                   (DApp (DApp factorialLoop (DInt n)) (DInt 3)))
        [0, 5]
  ]
