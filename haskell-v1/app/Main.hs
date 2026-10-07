module Main where

import System.Environment
import System.Exit
import Data.List (foldl', isPrefixOf, partition)
import Control.Monad (when)
import Text.PrettyPrint (render)

import Ast
import Interpret
import Template
import Parse
import Typecheck

data Flags = Flags
 { verbose :: Bool
 , onlyExpand :: Bool
 }

defaultFlags :: Flags
defaultFlags = Flags
  { verbose = True
  , onlyExpand = False
  }

applyFlag :: Flags -> String -> Flags
applyFlag f ("-v") = f { verbose = True }
applyFlag f ("-verbose") = f { verbose = True }
applyFlag f ("-no-verbose") = f { verbose = False }
applyFlag f ("-only-expand") = f { onlyExpand = True }
applyFlag f ("-no-only-expand") = f { onlyExpand = False }
applyFlag _ s = error $ "Unknown flag " ++ s

-- TODO rework all of this with exitWith instead of error

main :: IO ()
main = do
  args <- getArgs
  let (flagArgs, otherArgs) = partition (\a -> isPrefixOf "-" a) args
  let flags = foldl' applyFlag defaultFlags flagArgs

  let source = case otherArgs of
        [s] -> s
        x -> error $ "Need exactly one source file, got " ++ show x
  code <- readFile source
  program <- case parseWhole code of
    Left e -> error $ show e
    Right p -> pure $ p
  when (verbose flags) $
    putStrLn $ "Parsed:\n" ++ (render $ pp program)


  (_, tc) <-
    case runTT (typecheck program) of
      Left err -> (putStrLn ("Typechecking Error:\n" ++ show err)) >>
        exitWith (ExitFailure 1)
      Right e -> pure e
  when (verbose flags) $
    putStrLn $ "\nTypechecked:\n" ++ (render $ pp tc)

  te <-
    case runM (templateExpand tc) of
      Left err -> (putStrLn ("Template Error:\n" ++ show err)) >>
        error (show err)
      Right e -> pure e
  when (verbose flags) $
    putStrLn $ "\nTemplates expanded:\n" ++ (render $ pp te)

  (_, tc2@(DProgram eBinds eBody)) <-
    case runTT (typecheck te) of
      Left err -> (putStrLn ("Typechecking Error:\n" ++ show err)) >>
        exitWith (ExitFailure 1)
      Right e -> pure e
  when (verbose flags) $
    putStrLn $ "\nTypechecked again:\n" ++ (render $ pp tc2)


  when (not $ onlyExpand flags) $ do
    v <- case runM (d2expLet [] eBinds (Just eBody)) of
      Left err -> (putStrLn ("Interpret Error:\n" ++ show err)) >>
         error (show err)
      Right v -> pure v
    putStrLn $ "\nProduced Value:\n" ++ (render $ pp v)
