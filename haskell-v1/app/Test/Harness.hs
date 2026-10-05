module Test.Harness where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless)
import Data.IORef (modifyIORef', newIORef, readIORef)

import Ast

data TestCase = TestCase
  { caseName :: String
  , caseRun :: IO ()
  }

testCase :: String -> IO () -> TestCase
testCase = TestCase

-- Stand-in for python's S2E000, used as a don't-care type annotation.
anyType :: SExp
anyType = SCon "any" []

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
  unless (expected == actual) $
    ioError . userError $
      label ++ ": expected " ++ show expected ++ ", got " ++ show actual

assertValue :: (Eq a, Show a) => String -> a -> M a -> IO ()
assertValue label expected result =
  case runM result of
    Right actual -> assertEqual label expected actual
    Left _ -> ioError . userError $ label ++ ": unexpected error"

assertLeft :: String -> M a -> IO ()
assertLeft label result =
  case runM result of
    Left _ -> pure ()
    Right _ -> ioError . userError $ label ++ ": expected an error, but succeeded"

unwrap :: String -> M a -> IO a
unwrap label result =
  case runM result of
    Right value -> pure value
    Left _ -> ioError . userError $ label ++ ": unexpected error"

runSuite :: String -> [TestCase] -> IO Bool
runSuite name cases = do
  putStrLn (name ++ ":")
  failures <- newIORef (0 :: Int)
  forM_ cases $ \tc -> do
    outcome <- try (caseRun tc) :: IO (Either SomeException ())
    case outcome of
      Right () -> putStrLn ("  ok    " ++ caseName tc)
      Left e -> do
        modifyIORef' failures (+ 1)
        putStrLn ("  FAIL  " ++ caseName tc)
        putStrLn ("        " ++ show e)
  count <- readIORef failures
  putStrLn ("  " ++ show (length cases - count) ++ " of " ++
            show (length cases) ++ " passed")
  pure (count == 0)
