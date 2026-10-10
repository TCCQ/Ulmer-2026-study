module Test (main) where

import Control.Monad (unless)
import System.Exit (exitFailure)

import Test.Harness
import qualified Test.Interp00
import qualified Test.Interp01
import qualified Test.Template
import qualified Test.Typecheck

main :: IO ()
main = do
  putStrLn "FWATS3 level-2 tests"
  passed <- mapM (uncurry runSuite)
    [ ("interp00", Test.Interp00.tests)
    , ("interp01", Test.Interp01.tests)
    , ("template", Test.Template.tests)
    , ("typecheck", Test.Typecheck.tests)
    ]
  unless (and passed) exitFailure
