module Ast.MError where

data Error
  = ErrorGeneric String
  | ErrorName String
  | ErrorContext Error String

instance Show Error where
  show (ErrorGeneric e) = e
  show (ErrorName n) = "Unknown name " ++ n ++ " encountered"
  show (ErrorContext e s) = show e ++ '\n':s
