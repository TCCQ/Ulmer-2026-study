module Test.Typecheck (tests) where

import Ast
import Typecheck
import Test.Harness

assertType :: String -> SExp -> TT (SExp, a) -> IO ()
assertType label expected result =
  case runTT result of
    Right (actual, _) -> assertEqual label expected actual
    Left e -> ioError . userError $ label ++ ": unexpected error\n" ++ show e

assertTypeLeft :: String -> TT a -> IO ()
assertTypeLeft label result =
  case runTT result of
    Left _ -> pure ()
    Right _ -> ioError . userError $ label ++ ": expected an error, but succeeded"

tests :: [TestCase]
tests =
  [ testCase "integer literal has type Int" $
      assertType "int literal" intType $
        typecheck (DInt 1)

  , testCase "boolean literal has type Bool" $
      assertType "bool literal" boolType $
        typecheck (DBool True)

  , testCase "string literal has type String" $
      assertType "string literal" stringType $
        typecheck (DStr "hello")

  , testCase "arithmetic operators have type Int" $
      mapM_ (\op -> assertType ("operator " ++ op) intType $
                       typecheck (DOp2 op (DInt 1) (DInt 2)))
        ["+", "-", "*"]

  , testCase "comparison operators have type Bool" $
      mapM_ (\op -> assertType ("operator " ++ op) boolType $
                       typecheck (DOp2 op (DInt 1) (DInt 2)))
        ["<", ">", "<=", ">=", "==", "!="]

  , testCase "nested binary operators" $
      assertType "nested arithmetic" intType $
        typecheck (DOp2 "+" (DOp2 "*" (DInt 2) (DInt 3)) (DInt 1))

  , testCase "binary operator with a wrong operand type is rejected" $
      assertTypeLeft "adding an int and a bool" $
        typecheck (DOp2 "+" (DInt 1) (DBool True))

  , testCase "unknown binary operator is rejected" $
      assertTypeLeft "unknown operator" $
        typecheck (DOp2 "^" (DInt 1) (DInt 2))

  , testCase "bound variable takes its binding type" $
      assertType "bound variable" intType $
        extend (insertName "x" intType) (typecheck (DVar "x"))

  , testCase "unbound variable is rejected" $
      assertTypeLeft "unbound variable" $
        typecheck (DVar "missing")

  , testCase "lambda has a function type" $ do
      assertType "identity lambda" (SFun intType intType) $
        typecheck (DLam ("x", intType) (DVar "x"))
      assertType "constant lambda" (SFun intType stringType) $
        typecheck (DLam ("x", intType) (DStr "ignored"))

  , testCase "lambda body type errors are rejected" $
      assertTypeLeft "lambda body fails" $
        typecheck (DLam ("x", intType) (DOp2 "+" (DVar "x") (DStr "a")))

  , testCase "lambda body cannot see unknown names" $
      assertTypeLeft "lambda body names an unknown" $
        typecheck (DLam ("x", intType) (DVar "y"))

  , testCase "fix has its declared function type" $
      assertType "recursive factorial" (SFun intType intType) $
        typecheck
          (DFix "f" ("n", intType)
             (DIf (DOp2 "==" (DVar "n") (DInt 0))
                  (DInt 1)
                  (DOp2 "*" (DVar "n")
                            (DApp (DVar "f")
                                  (DOp2 "-" (DVar "n") (DInt 1)))))
             (SFun intType intType))

  , testCase "fix whose declared type is not a function is rejected" $
      assertTypeLeft "fix declared as Int" $
        typecheck (DFix "f" ("n", intType) (DInt 1) intType)

  , testCase "fix whose body disagrees with its declared type is rejected" $
      assertTypeLeft "fix body is a string" $
        typecheck (DFix "f" ("n", intType) (DStr "x") (SFun intType intType))

  , testCase "application returns the function result type" $ do
      assertType "identity applied" intType $
        typecheck (DApp (DLam ("x", intType) (DVar "x")) (DInt 5))
      assertType "curried application" (SFun stringType intType) $
        typecheck
          (DApp (DLam ("x", intType) (DLam ("y", stringType) (DVar "x")))
                (DInt 5))

  , testCase "application with the wrong argument type is rejected" $
      assertTypeLeft "int function given a string" $
        typecheck (DApp (DLam ("x", intType) (DVar "x")) (DStr "a"))

  , testCase "application of a non-function is rejected" $
      assertTypeLeft "applying an int" $
        typecheck (DApp (DInt 1) (DInt 2))

  , testCase "if returns the branch type" $
      assertType "if over ints" intType $
        typecheck (DIf (DBool True) (DInt 1) (DInt 2))

  , testCase "if on a non-boolean is rejected" $
      assertTypeLeft "if over an int condition" $
        typecheck (DIf (DInt 1) (DInt 1) (DInt 2))

  , testCase "if with mismatched branches is rejected" $
      assertTypeLeft "if branches disagree" $
        typecheck (DIf (DBool True) (DInt 1) (DStr "a"))

  , testCase "tuple has the tuple of element types" $
      assertType "mixed tuple" (STuple [intType, boolType, stringType]) $
        typecheck (DTuple [DInt 1, DBool True, DStr "s"])

  , testCase "nested tuple has a nested tuple type" $
      assertType "nested tuple" (STuple [intType, STuple [stringType]]) $
        typecheck (DTuple [DInt 1, DTuple [DStr "s"]])

  , testCase "projection selects an element type" $ do
      assertType "first of two" intType $
        typecheck (DProj 0 (DTuple [DInt 1, DStr "s"]))
      assertType "second of two" stringType $
        typecheck (DProj 1 (DTuple [DInt 1, DStr "s"]))

  , testCase "projection on a non-tuple is rejected" $
      assertTypeLeft "projecting an int" $
        typecheck (DProj 0 (DInt 1))

  , testCase "projection past the tuple is rejected" $
      assertTypeLeft "projecting past the end" $
        typecheck (DProj 5 (DTuple [DInt 1, DStr "s"]))

  , testCase "let returns the body type" $
      assertType "body is a string" stringType $
        typecheck (DLet [DBind "x" (DInt 1)] (Just (DStr "s")))

  , testCase "let without a body is the unit type" $ do
      assertType "bare declarations" unitType $
        typecheck (DLet [DBind "x" (DInt 1)] Nothing)
      assertType "empty let" unitType $
        typecheck (DLet [] Nothing)

  , testCase "let with no declarations returns the body type" $
      assertType "empty declarations" intType $
        typecheck (DLet [] (Just (DInt 1)))

  , testCase "let bindings are visible to later bindings and the body" $
      assertType "sequential bindings" intType $
        typecheck (DLet [ DBind "x" (DInt 1)
                        , DBind "y" (DVar "x")
                        ]
                        (Just (DVar "y")))

  , testCase "let binding shadows an earlier binding" $
      assertType "shadowed binding" stringType $
        typecheck (DLet [ DBind "x" (DInt 1)
                        , DBind "x" (DStr "s")
                        ]
                        (Just (DVar "x")))

  , testCase "let binding type errors are rejected" $
      assertTypeLeft "binding right-hand side fails" $
        typecheck (DLet [DBind "x" (DOp2 "+" (DInt 1) (DStr "a"))]
                       (Just (DVar "x")))

  , testCase "bind declaration records its right-hand type" $
      assertType "bind type" intType $
        typecheck (DBind "x" (DInt 1))

  , testCase "template declaration records its return type" $
      assertType "template return" (SFun (SVar "a") intType) $
        typecheck (DTDec (TDecl ("f", ["a"], SFun (SVar "a") intType)))

  , testCase "template declaration with a free return variable is rejected" $
      assertTypeLeft "return mentions an unbound variable" $
        typecheck (DTDec (TDecl ("f", ["a"], SVar "b")))

  , testCase "matching template redeclaration is allowed" $
      assertType "duplicated declaration" intType $
        typecheck (DProgram
          [ DTDec (TDecl ("t", [], intType))
          , DTDec (TDecl ("t", [], intType))
          ]
          (DInt 0))

  , testCase "conflicting template redeclaration is rejected" $
      assertTypeLeft "declarations disagree" $
        typecheck (DProgram
          [ DTDec (TDecl ("t", [], intType))
          , DTDec (TDecl ("t", [], stringType))
          ]
          (DInt 0))

  , testCase "implementation requires a prior declaration" $
      assertTypeLeft "implementation without a declaration" $
        typecheck (DImpl (TImpl ("f", DInt 1, [], intType, 0)))

  , testCase "implementation matching its declaration typechecks" $
      assertType "zero implementation" intType $
        typecheck (DProgram
          [ DTDec (TDecl ("zero", [], intType))
          , DImpl (TImpl ("zero", DInt 0, [], intType, 0))
          ]
          (DInt 0))

  , testCase "implementation whose body disagrees with its annotation is rejected" $
      assertTypeLeft "body is a string but the annotation says Int" $
        typecheck (DProgram
          [ DTDec (TDecl ("zero", [], intType))
          , DImpl (TImpl ("zero", DStr "s", [], intType, 0))
          ]
          (DInt 0))

  , testCase "implementation whose instantiated return type disagrees is rejected" $
      assertTypeLeft "implementation return type disagrees" $
        typecheck (DProgram
          [ DTDec (TDecl ("zero", [], intType))
          , DImpl (TImpl ("zero", DInt 0, [], stringType, 0))
          ]
          (DInt 0))

  , testCase "generic implementation typechecks" $
      assertType "identity implementation" intType $
        typecheck (DProgram
          [ DTDec (TDecl ("id", ["a"], SFun (SVar "a") (SVar "a")))
          , DImpl (TImpl ( "id"
                         , DLam ("x", SVar "a") (DVar "x")
                         , [SVar "a"]
                         , SFun (SVar "a") (SVar "a")
                         , 1 ))
          ]
          (DInt 0))

  , testCase "type application substitutes into the return type" $ do
      assertType "identity at Int" intType $
        typecheck (DProgram
          [ DTDec (TDecl ("id", ["a"], SVar "a")) ]
          (DTapp "id" [intType]))
      assertType "pair at Int and String"
        (STuple [intType, stringType]) $
        typecheck (DProgram
          [ DTDec (TDecl ("pair", ["a", "b"],
                          STuple [SVar "a", SVar "b"])) ]
          (DTapp "pair" [intType, stringType]))

  , testCase "type application of an unknown template is rejected" $
      assertTypeLeft "no declaration for the template" $
        typecheck (DTapp "missing" [intType])

  , testCase "program type is the body type" $
      assertType "program body type" stringType $
        typecheck (DProgram [DBind "x" (DInt 1)] (DStr "s"))

  , testCase "program declarations are visible to later declarations and the body" $
      assertType "chained bindings" intType $
        typecheck (DProgram
          [ DBind "x" (DInt 1)
          , DBind "y" (DVar "x")
          ]
          (DVar "y"))

  , testCase "program declaration errors are rejected" $
      assertTypeLeft "bind names an unknown" $
        typecheck (DProgram [DBind "x" (DVar "missing")] (DInt 0))

  , testCase "local exports are visible to the body" $
      assertType "exported binding" intType $
        typecheck (DProgram
          [ DLocal [DBind "hidden" (DStr "s")] [DBind "exported" (DInt 1)] ]
          (DVar "exported"))

  , testCase "local private bindings do not escape" $
      assertTypeLeft "private binding is not visible" $
        typecheck (DProgram
          [ DLocal [DBind "hidden" (DInt 1)] [] ]
          (DVar "hidden"))
  ]
