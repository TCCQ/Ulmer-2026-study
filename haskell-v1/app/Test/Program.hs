{- | Helpers for testing whole programs from source text.

The srcgen2 suites run a program by parsing it, type-checking it and then
evaluating it (@program_tcheck@ followed by @program_evaluate@).  The Haskell
suites mostly build syntax trees by hand; the helpers here add the missing
program-level view so those behaviours can be tested as they are written.

The pipeline is the one 'Main' uses:

> parse, bind names, type-check, expand templates and strip them,
> type-check again, then evaluate the remaining declarations and body.
-}
module Test.Program
  ( runSource
  , checkSource
  , assertRun
  , assertRunLeft
  , assertSourceType
  , assertSourceLeft
  ) where

import Ast
import Bind (bindProgram)
import Interpret
import Parse (parseWhole)
import Template
import Typecheck

import Test.Harness (assertEqual)

-- | Render a failure with 'show', so the various internal error types do not
-- have to agree on one representation.
withError :: Show e => Either e a -> Either String a
withError = either (Left . show) Right

-- | Type-check and evaluate a program, exactly as 'Main' does.
runSource :: String -> Either String DVal
runSource source = do
  program <- withError (parseWhole source)
  (nextI, program') <- withError (bindProgram program)
  (nextI2, (_, program'')) <- withError (evalTT (typecheck program') nextI)
  (nextI3, program3) <-
    withError (evalM (stripProgram =<< expandProgram program'') nextI2)
  (nextI4, (_, DProgram eBinds eBody)) <-
    withError (evalTT (typecheck program3) nextI3)
  (_, value) <- withError (evalM (d2expLet [] eBinds (Just eBody)) nextI4)
  Right value

-- | Type-check a program and answer with the type of its body.
checkSource :: String -> Either String (SExp Name)
checkSource source = do
  program <- withError (parseWhole source)
  (nextI, program') <- withError (bindProgram program)
  (_, (bodyType, _)) <- withError (evalTT (typecheck program') nextI)
  Right bodyType

assertRun :: String -> DVal -> IO ()
assertRun source expected =
  assertEqual ("running\n" ++ source) (Right expected) (runSource source)

assertRunLeft :: String -> IO ()
assertRunLeft source =
  case runSource source of
    Left _ -> pure ()
    Right value ->
      ioError . userError $
        "expected an error, but\n" ++ source ++ "\nran to " ++ show value

assertSourceType :: String -> SExp Name -> IO ()
assertSourceType source expected =
  assertEqual ("checking\n" ++ source) (Right expected) (checkSource source)

assertSourceLeft :: String -> IO ()
assertSourceLeft source =
  case checkSource source of
    Left _ -> pure ()
    Right bodyType ->
      ioError . userError $
        "expected a type error, but\n" ++ source ++ "\nchecked as " ++
        show bodyType
