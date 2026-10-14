module Test.Typecheck (tests) where

import Ast
import Typecheck
import Test.Harness

assertType :: String -> SExp Name -> TT (SExp Name, a) -> IO ()
assertType label expected result =
  case evalTT result 0 of
    Right (_, (actual, _)) -> assertEqual label expected actual
    Left e -> ioError . userError $ label ++ ": unexpected error\n" ++ show e

assertTypeLeft :: String -> TT a -> IO ()
assertTypeLeft label result =
  case evalTT result 0 of
    Left _ -> pure ()
    Right _ -> ioError . userError $ label ++ ": expected an error, but succeeded"

-- | Typecheck an expression.  Pinning the name parameter here keeps GHC from
-- having to guess it for the tests whose expressions mention no names.
tc :: DExp Name -> TT (SExp Name, DExp Name)
tc = typecheck

tests :: [TestCase]
tests =
  [ testCase "integer literal has type Int" $
      assertType "int literal" intType $
        tc (DInt 1)

  , testCase "boolean literal has type Bool" $
      assertType "bool literal" boolType $
        tc (DBool True)

  , testCase "string literal has type String" $
      assertType "string literal" stringType $
        tc (DStr "hello")

  , testCase "arithmetic operators have type Int" $
      mapM_ (\op -> assertType ("operator " ++ op) intType $
                       tc (DOp2 (testName op) (DInt 1) (DInt 2)))
        ["+", "-", "*"]

  , testCase "comparison operators have type Bool" $
      mapM_ (\op -> assertType ("operator " ++ op) boolType $
                       tc (DOp2 (testName op) (DInt 1) (DInt 2)))
        ["<", ">", "<=", ">=", "==", "!="]

  , testCase "nested binary operators" $
      assertType "nested arithmetic" intType $
        tc (DOp2 (testName "+")
                 (DOp2 (testName "*") (DInt 2) (DInt 3)) (DInt 1))

  , testCase "binary operator with a wrong operand type is rejected" $
      assertTypeLeft "adding an int and a bool" $
        tc (DOp2 (testName "+") (DInt 1) (DBool True))

  , testCase "unknown binary operator is rejected" $
      assertTypeLeft "unknown operator" $
        tc (DOp2 (testName "^") (DInt 1) (DInt 2))

  , testCase "bound variable takes its binding type" $
      assertType "bound variable" intType $
        extend (insertName (testName "x") intType) (tc (DVar (testName "x")))

  , testCase "unbound variable is rejected" $
      assertTypeLeft "unbound variable" $
        tc (DVar (testName "missing"))

  , testCase "lambda has a function type" $ do
      assertType "identity lambda" (SFun intType intType) $
        tc (DLam (testName "x", intType) (DVar (testName "x")))
      assertType "constant lambda" (SFun intType stringType) $
        tc (DLam (testName "x", intType) (DStr "ignored"))

  , testCase "lambda body type errors are rejected" $
      assertTypeLeft "lambda body fails" $
        tc (DLam (testName "x", intType)
             (DOp2 (testName "+") (DVar (testName "x")) (DStr "a")))

  , testCase "lambda body cannot see unknown names" $
      assertTypeLeft "lambda body names an unknown" $
        tc (DLam (testName "x", intType) (DVar (testName "y")))

  , testCase "fix has its declared function type" $
      assertType "recursive factorial" (SFun intType intType) $
        tc
          (DFix (testName "f") (testName "n", intType)
             (DIf (DOp2 (testName "==") (DVar (testName "n")) (DInt 0))
                  (DInt 1)
                  (DOp2 (testName "*") (DVar (testName "n"))
                            (DApp (DVar (testName "f"))
                                  (DOp2 (testName "-") (DVar (testName "n"))
                                        (DInt 1)))))
             (SFun intType intType))

  , testCase "fix whose declared type is not a function is rejected" $
      assertTypeLeft "fix declared as Int" $
        tc (DFix (testName "f") (testName "n", intType) (DInt 1) intType)

  , testCase "fix whose body disagrees with its declared type is rejected" $
      assertTypeLeft "fix body is a string" $
        tc (DFix (testName "f") (testName "n", intType)
              (DStr "x") (SFun intType intType))

  , testCase "application returns the function result type" $ do
      assertType "identity applied" intType $
        tc (DApp (DLam (testName "x", intType) (DVar (testName "x")))
                 (DInt 5))
      assertType "curried application" (SFun stringType intType) $
        tc
          (DApp (DLam (testName "x", intType)
                  (DLam (testName "y", stringType) (DVar (testName "x"))))
                (DInt 5))

  , testCase "application with the wrong argument type is rejected" $
      assertTypeLeft "int function given a string" $
        tc (DApp (DLam (testName "x", intType) (DVar (testName "x")))
                 (DStr "a"))

  , testCase "application of a non-function is rejected" $
      assertTypeLeft "applying an int" $
        tc (DApp (DInt 1) (DInt 2))

  , testCase "if returns the branch type" $
      assertType "if over ints" intType $
        tc (DIf (DBool True) (DInt 1) (DInt 2))

  , testCase "if on a non-boolean is rejected" $
      assertTypeLeft "if over an int condition" $
        tc (DIf (DInt 1) (DInt 1) (DInt 2))

  , testCase "if with mismatched branches is rejected" $
      assertTypeLeft "if branches disagree" $
        tc (DIf (DBool True) (DInt 1) (DStr "a"))

  , testCase "tuple has the tuple of element types" $
      assertType "mixed tuple" (STuple [intType, boolType, stringType]) $
        tc (DTuple [DInt 1, DBool True, DStr "s"])

  , testCase "nested tuple has a nested tuple type" $
      assertType "nested tuple" (STuple [intType, STuple [stringType]]) $
        tc (DTuple [DInt 1, DTuple [DStr "s"]])

  , testCase "projection selects an element type" $ do
      assertType "first of two" intType $
        tc (DProj 0 (DTuple [DInt 1, DStr "s"]))
      assertType "second of two" stringType $
        tc (DProj 1 (DTuple [DInt 1, DStr "s"]))

  , testCase "projection on a non-tuple is rejected" $
      assertTypeLeft "projecting an int" $
        tc (DProj 0 (DInt 1))

  , testCase "projection past the tuple is rejected" $
      assertTypeLeft "projecting past the end" $
        tc (DProj 5 (DTuple [DInt 1, DStr "s"]))

  , testCase "let returns the body type" $
      assertType "body is a string" stringType $
        tc (DLet [DBind (testName "x") (DInt 1)] (Just (DStr "s")))

  , testCase "let without a body is the unit type" $ do
      assertType "bare declarations" unitType $
        tc (DLet [DBind (testName "x") (DInt 1)] Nothing)
      assertType "empty let" unitType $
        tc (DLet [] Nothing)

  , testCase "let with no declarations returns the body type" $
      assertType "empty declarations" intType $
        tc (DLet [] (Just (DInt 1)))

  , testCase "let bindings are visible to later bindings and the body" $
      assertType "sequential bindings" intType $
        tc (DLet [ DBind (testName "x") (DInt 1)
                 , DBind (testName "y") (DVar (testName "x"))
                 ]
                 (Just (DVar (testName "y"))))

  , testCase "let binding shadows an earlier binding" $
      assertType "shadowed binding" stringType $
        tc (DLet [ DBind (testName "x") (DInt 1)
                 , DBind (testName "x") (DStr "s")
                 ]
                 (Just (DVar (testName "x"))))

  , testCase "let binding type errors are rejected" $
      assertTypeLeft "binding right-hand side fails" $
        tc (DLet [DBind (testName "x")
                    (DOp2 (testName "+") (DInt 1) (DStr "a"))]
                (Just (DVar (testName "x"))))

  , testCase "bind declaration records its right-hand type" $
      assertType "bind type" intType $
        typecheck (DBind (testName "x") (DInt 1))

  , testCase "template declaration records its return type" $
      assertType "template return" (SFun (SVar (testName "a")) intType) $
        typecheck (DTDec (TDecl (testName "f", [testName "a"],
                                 SFun (SVar (testName "a")) intType)))

  , testCase "template declaration with a free return variable is rejected" $
      assertTypeLeft "return mentions an unbound variable" $
        typecheck (DTDec (TDecl (testName "f", [testName "a"], SVar (testName "b"))))

  , testCase "matching template redeclaration is allowed" $
      assertType "duplicated declaration" intType $
        typecheck (DProgram
          [ DTDec (TDecl (testName "t", [], intType))
          , DTDec (TDecl (testName "t", [], intType))
          ]
          (DInt 0))

  , testCase "conflicting template redeclaration is rejected" $
      assertTypeLeft "declarations disagree" $
        typecheck (DProgram
          [ DTDec (TDecl (testName "t", [], intType))
          , DTDec (TDecl (testName "t", [], stringType))
          ]
          (DInt 0))

  , testCase "implementation requires a prior declaration" $
      assertTypeLeft "implementation without a declaration" $
        typecheck (DImpl (TImpl (testName "f", [], DInt 1, [], 0)))

  , testCase "implementation matching its declaration typechecks" $
      assertType "zero implementation" intType $
        typecheck (DProgram
          [ DTDec (TDecl (testName "zero", [], intType))
          , DImpl (TImpl (testName "zero", [], DInt 0, [], 0))
          ]
          (DInt 0))

  , testCase "implementation whose body disagrees with its declaration is rejected" $
      assertTypeLeft "body is a string but the declaration says Int" $
        typecheck (DProgram
          [ DTDec (TDecl (testName "zero", [], intType))
          , DImpl (TImpl (testName "zero", [], DStr "s", [], 0))
          ]
          (DInt 0))

  , testCase "generic implementation typechecks" $
      assertType "identity implementation" intType $
        typecheck (DProgram
          [ DTDec (TDecl (testName "id", [testName "a"],
                          SFun (SVar (testName "a")) (SVar (testName "a"))))
          , DImpl (TImpl ( testName "id"
                         , [testName "a"]
                         , DLam (testName "x", SVar (testName "a"))
                                (DVar (testName "x"))
                         , [SVar (testName "a")]
                         , 1 ))
          ]
          (DInt 0))

  , testCase "type application substitutes into the return type" $ do
      assertType "identity at Int" intType $
        typecheck (DProgram
          [ DTDec (TDecl (testName "id", [testName "a"], SVar (testName "a"))) ]
          (DTapp (testName "id") [intType]))
      assertType "pair at Int and String"
        (STuple [intType, stringType]) $
        typecheck (DProgram
          [ DTDec (TDecl (testName "pair", [testName "a", testName "b"],
                          STuple [SVar (testName "a"), SVar (testName "b")])) ]
          (DTapp (testName "pair") [intType, stringType]))

  , testCase "type application of an unknown template is rejected" $
      assertTypeLeft "no declaration for the template" $
        tc (DTapp (testName "missing") [intType])

  , testCase "program type is the body type" $
      assertType "program body type" stringType $
        typecheck (DProgram [DBind (testName "x") (DInt 1)] (DStr "s"))

  , testCase "program declarations are visible to later declarations and the body" $
      assertType "chained bindings" intType $
        typecheck (DProgram
          [ DBind (testName "x") (DInt 1)
          , DBind (testName "y") (DVar (testName "x"))
          ]
          (DVar (testName "y")))

  , testCase "program declaration errors are rejected" $
      assertTypeLeft "bind names an unknown" $
        typecheck (DProgram [DBind (testName "x") (DVar (testName "missing"))] (DInt 0))

  , testCase "local exports are visible to the body" $
      assertType "exported binding" intType $
        typecheck (DProgram
          [ DLocal [DBind (testName "hidden") (DStr "s")]
                   [DBind (testName "exported") (DInt 1)] ]
          (DVar (testName "exported")))

  , testCase "local private bindings do not escape" $
      assertTypeLeft "private binding is not visible" $
        typecheck (DProgram
          [ DLocal [DBind (testName "hidden") (DInt 1)] [] ]
          (DVar (testName "hidden")))
  ]
