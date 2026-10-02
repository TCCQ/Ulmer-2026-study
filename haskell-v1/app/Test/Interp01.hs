module Test.Interp01 (tests) where

import Ast
import Interpret
import Test.Harness

factorialLoop :: DExp
factorialLoop =
  DFix "loop" ("n", anyType)
    (DLam ("acc", anyType)
      (DIf (DOp2 "==" (DVar "n") (DInt 0))
           (DVar "acc")
           (DApp (DApp (DVar "loop") (DOp2 "-" (DVar "n") (DInt 1)))
                 (DOp2 "*" (DVar "n") (DVar "acc")))))
    anyType

tuple3 :: DExp -> DExp -> DExp -> DExp
tuple3 first second third = DTuple [first, second, third]

fibonacciLoop :: DExp
fibonacciLoop =
  DFix "fibo" ("state", anyType)
    (DIf (DOp2 "==" n (DInt 0))
         a
         (DApp (DVar "fibo")
               (tuple3 (DOp2 "-" n (DInt 1)) b (DOp2 "+" a b))))
    anyType
  where
    state = DVar "state"
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
          (DLet [ DBind "n" (DInt 10)
                , DBind "fib" fibonacciLoop
                , DBind "initial"
                    (tuple3 (DVar "n") (DInt 0) (DInt 1))
                ]
                (Just (DApp (DVar "fib") (DVar "initial"))))

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
