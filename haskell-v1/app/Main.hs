module Main where

import System.Environment
import Data.List (foldl', isPrefixOf, partition)
import Control.Monad (when)

import Ast
import Interpret
import Template
import Parse

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
    putStrLn $ "Parsed:\n" ++ show program
  (DProgram eBinds eBody) <-
    case runM (templateExpand program) of
      Left err -> (putStrLn ("Template Error:\n" ++ show err)) >>
        error (show err)
      Right e -> pure e
  v <- case runM (d2expLet [] eBinds (Just eBody)) of
    Left err -> (putStrLn ("Interpret Error:\n" ++ show err)) >>
       error (show err)
    Right v -> pure v
  putStrLn $ "Produced Value:\n" ++ show v
