{- | Behavioural tests for the concrete syntax, mirroring
@contrib/githwxi/FWATS3/srcgen2/tests/test_parser.py@.

The Haskell syntax differs from srcgen2's in a few places, so the mirrors are
adapted where they have to be:

* type names are @Int@, @Bool@ and @String@ (srcgen2: @sint@, @bool@,
  @string@);
* @f[]@ is written @\@f[]@, because a static application needs the @\@@;
* the annotation on @fix@ is the whole function type
  (@fix f(n: Int): Int -> Int@), not just the return type;
* parenthesised type constructors (@list(Int)@) are not part of the grammar,
  only the bracket form @list[Int]@;
* there is no library-file parser, only whole programs.

The cases that cannot be mirrored for those reasons are recorded in
@haskell-v1/TEST-PARITY.md@.
-}
module Test.Parse (tests) where

import Control.Monad (forM_)

import Ast
import Parse
import Test.Harness
import Text.Parsec (ParseError)
import Text.Parsec.Error (errorPos)
import Text.Parsec.Pos (sourceLine)

-- | Parse an expression.
pe :: String -> Either ParseError (DExp String)
pe = parseExprString

-- | Parse a type.
pt :: String -> Either ParseError (SExp String)
pt = parseTypeString

-- | Parse a whole program.
pw :: String -> Either ParseError (DProgram String)
pw = parseWhole

-- | 'ParseError' has no 'Eq', so compare through its rendering.
assertParses :: (Eq a, Show a) => String -> a -> Either ParseError a -> IO ()
assertParses label expected actual =
  assertEqual label (Right expected) (either (Left . show) Right actual)

assertParseError :: String -> Either ParseError a -> IO ()
assertParseError label result =
  case result of
    Left _ -> pure ()
    Right _ -> ioError . userError $ label ++ ": expected a parse error"

expectProgram :: String -> (DProgram String -> IO ()) -> IO ()
expectProgram source check =
  case pw source of
    Left err -> ioError . userError $ "could not parse:\n" ++ source ++
                "\n" ++ show err
    Right program -> check program

tests :: [TestCase]
tests =
  [ testCase "type grouping and arrows" $ do
      assertParses "((a)) is a" (SCon "a" []) (pt "((a))")
      assertParses "(a,) is a one-tuple" (STuple [SCon "a" []]) (pt "(a,)")
      assertParses "() is the empty tuple" (STuple []) (pt "()")
      assertParses "a" (SCon "a" []) (pt "a")
      assertParses "Int[] is Int" (SCon "Int" []) (pt "Int[]")
      assertParses "arrows group to the right"
        (SFun (SCon "a" []) (SFun (SCon "b" []) (SCon "c" [])))
        (pt "a -> b -> c")
      assertParses "a trailing comma in a constructor"
        (SCon "list" [SCon "a" []]) (pt "list[a,]")
      -- The same names inside a declaration are static variables instead.
      expectProgram "dec f[a]: (a,)\n0" $ \program ->
        case program of
          DProgram [DTDec (TDecl (name, args, signature))] _ -> do
            assertEqual "declaration name" "f" name
            assertEqual "declaration parameters" ["a"] args
            assertEqual "a is a static variable in scope" (STuple [SVar "a"])
              signature
          _ -> ioError (userError "unexpected program shape")

  , testCase "parenthesised type constructors are rejected" $ do
      -- srcgen2 reads list(sint) as sugar for list[sint]; the Haskell grammar
      -- only has the bracket form.
      assertParses "the bracket form parses"
        (SCon "list" [SCon "Int" []]) (pt "list[Int]")
      assertParseError "the parenthesised form is a syntax error"
        (pt "list(Int)")

  , testCase "precedence and associativity" $ do
      assertParses "arithmetic binds tighter than comparison"
        (DOp2 "<"
              (DOp2 "-"
                    (DOp2 "+" (DOp1 "-" (DInt 1))
                          (DOp2 "*" (DInt 2) (DInt 3)))
                    (DOp2 "/" (DInt 4) (DOp1 "+" (DInt 2))))
              (DInt 9))
        (pe "-1 + 2 * 3 - 4 / +2 < 9")
      forM_ ["<", ">", "<=", ">=", "==", "!="] $ \op ->
        assertParses ("the comparison " ++ op)
          (DOp2 op (DVar "x") (DVar "y"))
          (pe ("x " ++ op ++ " y"))

  , testCase "modulo precedence and associativity" $ do
      assertParses "% has the same precedence as * and /, left to right"
        (DOp2 "+" (DInt 1)
              (DOp2 "/" (DOp2 "*" (DOp2 "%" (DInt 20) (DInt 6)) (DInt 2))
                    (DInt 4)))
        (pe "1+20%6*2/4")
      assertParses "% associates to the left"
        (DOp2 "%"
              (DOp2 "%" (DOp2 "*" (DInt 20) (DInt 3)) (DInt 7))
              (DInt 2))
        (pe "20*3%7%2")
      assertParses "unary signs around %"
        (DOp2 "%" (DOp1 "-" (DInt 7)) (DOp1 "+" (DInt 3)))
        (pe "-7 % +3")
      -- The modulo implementation from the reference library.
      expectProgram
        "dec f1_mod: (Int, Int) -> Int;\nimp f1_mod = lam (xy: (Int, Int)) => xy.0 % xy.1\n0"
        $ \program ->
          case program of
            DProgram [_, DImpl (TImpl (_, _, DLam _ body, _, _))] _ ->
              assertEqual "the body is a modulo of the projections"
                (DOp2 "%" (DProj 0 (DVar "xy")) (DProj 1 (DVar "xy"))) body
            _ -> ioError (userError "unexpected program shape")

  , testCase "calls, tuples and postfix order" $ do
      assertParses "a call, then a projection, then another call"
        (DApp (DProj 0 (DApp (DTapp "f" [SCon "Int" []]) (DVar "x")))
              (DVar "y"))
        (pe "@f[Int](x).0(y)")
      assertParses "f() applies to the empty tuple"
        (DApp (DVar "f") (DTuple [])) (pe "f()")
      assertParses "f(x) applies to one argument"
        (DApp (DVar "f") (DVar "x")) (pe "f(x)")
      assertParses "f(x,) is a one-tuple argument"
        (DApp (DVar "f") (DTuple [DVar "x"])) (pe "f(x,)")
      assertParses "f(x,y,) is a two-tuple argument"
        (DApp (DVar "f") (DTuple [DVar "x", DVar "y"])) (pe "f(x,y,)")
      assertParses "a static application needs the @"
        (DTapp "f" []) (pe "@f[]")
      assertParseError "f[] without @ is a syntax error" (pe "f[]")

  , testCase "lexical rules" $ do
      assertParses "tab escape" (DStr "a\tb") (pe "\"a\\tb\"")
      assertParses "newline escape" (DStr "a\nb") (pe "\"a\\nb\"")
      assertParses "carriage-return escape" (DStr "a\rb") (pe "\"a\\rb\"")
      assertParses "backslash escape" (DStr "a\\b") (pe "\"a\\\\b\"")
      assertParses "quote escape" (DStr "a\"b") (pe "\"a\\\"b\"")
      assertParses "a comment ends the line" (DInt 1) (pe "1 # comment\n")
      assertParses "a comment between tokens"
        (DOp2 "+" (DInt 1) (DInt 2)) (pe "1 + 2 # trailing")
      assertParses "@foo$bar is one name"
        (DTapp "foo$bar" []) (pe "@foo$bar")
      assertParses "true is a literal" (DBool True) (pe "true")
      assertParses "false is a literal" (DBool False) (pe "false")
      assertParses "true$ is an ordinary variable"
        (DVar "true$") (pe "true$")
      -- The whole string from srcgen2's lexical test, with its escapes and its
      -- trailing comment.
      assertParses "escapes and a comment together"
        (DStr "a#b\n\t\r\\\"")
        (pe " \"a#b\\n\\t\\r\\\\\\\"\" # comment\n")

  , testCase "a signature and a tuple pattern" $
      expectProgram
        (unlines
           [ "dec identity[t]: t -> t;"
           , "imp{a,b} identity[(a,b)] = lam (x: (a,b)) => x;"
           , "@identity[(Int,Bool)](42, true)"
           ]) $ \program ->
        case program of
          DProgram [DTDec (TDecl (declName, declArgs, declSig)),
                    DImpl (TImpl (_, implVars, implBody, implPats, _))] body -> do
            assertEqual "declaration name" "identity" declName
            assertEqual "declaration parameters" ["t"] declArgs
            assertEqual "declaration signature" (SFun (SVar "t") (SVar "t"))
              declSig
            assertEqual "implementation parameters" ["a", "b"] implVars
            assertEqual "implementation patterns"
              [STuple [SVar "a", SVar "b"]] implPats
            assertEqual "implementation body"
              (DLam ("x", STuple [SVar "a", SVar "b"]) (DVar "x")) implBody
            assertEqual "the use"
              (DApp (DTapp "identity" [STuple [SCon "Int" [], SCon "Bool" []]])
                    (DTuple [DInt 42, DBool True]))
              body
          _ -> ioError (userError "unexpected program shape")

  , testCase "scopes are restored and outer parameters stay visible" $
      expectProgram
        (unlines
           [ "imp{a} outer[a] ="
           , "  let"
           , "    dec inner[b]: (a,b) -> a;"
           , "    imp{b} inner[b] = lam (x: a) => x"
           , "  in lam (x: a) => x end;"
           , "dec plain: a;"
           , "imp plain = lam (x: a) => x"
           , "0"
           ]) $ \program ->
        case program of
          DProgram
            [ DImpl (TImpl (_, _, DLet innerDecls (Just outerResult), _, _))
            , DTDec (TDecl (_, _, plainSig))
            , DImpl (TImpl (_, _, plainBody, _, _))] _ -> do
            case innerDecls of
              [DTDec (TDecl (_, _, innerSig)), _] ->
                assertEqual "the outer parameter is visible in the let signature"
                  (SFun (STuple [SVar "a", SVar "b"]) (SVar "a")) innerSig
              _ -> ioError (userError "unexpected let shape")
            assertEqual "the outer result uses the parameter"
              (DLam ("x", SVar "a") (DVar "x")) outerResult
            assertEqual "the outer scope is restored after the implementation"
              (SCon "a" []) plainSig
            assertEqual "and so is the annotation in the next declaration"
              (DLam ("x", SCon "a" []) (DVar "x")) plainBody
          _ -> ioError (userError "unexpected program shape")

  , testCase "fix, if and let" $ do
      assertParses "fix with an if body"
        (DFix "f" ("n", SCon "Int" [])
              (DIf (DOp2 ">" (DVar "n") (DInt 0))
                   (DApp (DVar "f") (DOp2 "-" (DVar "n") (DInt 1)))
                   (DInt 1))
              (SFun (SCon "Int" []) (SCon "Int" [])))
        (pe "fix f(n: Int): Int -> Int => if n > 0 then f(n - 1) else 1")
      assertParses "let end has no bound declarations and no body"
        (DLet [] Nothing) (pe "let end")
      assertParses "let in () end binds nothing and yields ()"
        (DLet [] (Just (DTuple []))) (pe "let in () end")
      assertParses "let val x = 1 in x end"
        (DLet [DBind "x" (DInt 1)] (Just (DVar "x")))
        (pe "let val x = 1 in x end")

  , testCase "local and empty lists" $ do
      expectProgram "local in end\n0" $ \program ->
        assertEqual "an empty local" (DProgram [DLocal [] []] (DInt 0))
          program
      expectProgram "dec x[]: Int; imp x[] = 1;\n0" $ \program ->
        case program of
          DProgram [DTDec (TDecl (declName, declArgs, declSig)),
                    DImpl (TImpl (implName, implVars, implBody, implPats, tag))]
                   body -> do
            assertEqual "declaration name" "x" declName
            assertEqual "declaration takes no parameters" [] declArgs
            assertEqual "declaration type" (SCon "Int" []) declSig
            assertEqual "implementation name" "x" implName
            assertEqual "implementation takes no static variables" [] implVars
            assertEqual "implementation patterns are empty" [] implPats
            assertEqual "implementation body" (DInt 1) implBody
            assertEqual "first implementation tag" 0 tag
            assertEqual "program body" (DInt 0) body
          _ -> ioError (userError "unexpected program shape")

  , testCase "the program boundary" $ do
      assertParses "42 is a program with no declarations"
        (DProgram [] (DInt 42)) (pw "42")
      assertEqual "a semicolon is optional before the body"
        (either (Left . show) Right (pw "val x = 1; x"))
        (either (Left . show) Right (pw "val x = 1 x"))
      forM_ ["", "val x = 1", "42;", "42 43"] $ \source ->
        assertParseError ("the program " ++ show source ++ " is rejected")
          (pw source)

  , testCase "an inlined library parses" $ do
      -- A Haskell-syntax equivalent of mylib/1.fwats3, which srcgen2 reads in
      -- its own test.  The fix annotation is the whole function type here.
      let library = unlines
            [ "dec f1_add[a]: (a, a) -> a;"
            , "imp f1_add[Int] = lam (xy: (Int, Int)) => xy.0 + xy.1;"
            , "dec f1_sub[a]: (a, a) -> a;"
            , "imp f1_sub[Int] = lam (xy: (Int, Int)) => xy.0 - xy.1;"
            , "dec f1_fact[a]: a -> a;"
            , "imp{a} f1_fact[a] ="
            , "  let"
            , "    val one = 1"
            , "  in"
            , "    fix fact(n: a): a -> a =>"
            , "      if n <= one then one else n * fact(n - one)"
            , "  end;"
            , "0"
            ]
      expectProgram library $ \program ->
        case program of
          DProgram decls _ -> do
            case [impl | DImpl impl <- decls] of
              [addImpl, subImpl, factImpl'] -> do
                assertEqual "f1_add is a lambda over a pair"
                  (DLam ("xy", STuple [SCon "Int" [], SCon "Int" []])
                        (DOp2 "+" (DProj 0 (DVar "xy"))
                                   (DProj 1 (DVar "xy"))))
                  (lamBody addImpl)
                case lamBody subImpl of
                  DLam _ result ->
                    assertEqual "f1_sub subtracts" "-" (opName result)
                  _ -> ioError (userError "f1_sub is not a lambda")
                case lamBody factImpl' of
                  DLet _ (Just (DFix _ ("n", argType) _ fixType)) -> do
                    assertEqual "the recursive argument is a"
                      (SVar "a") argType
                    assertEqual "the recursive annotation is a -> a"
                      (SFun (SVar "a") (SVar "a")) fixType
                  _ -> ioError (userError "f1_fact is not a let around a fix")
              _ -> ioError (userError "unexpected library shape")
          _ -> ioError (userError "unexpected program shape")

  , testCase "invalid expressions" $
      forM_ [ "lam(x) => x"
            , "fix f(x: Int) => x"
            , "if true then 1"
            , "1 < 2 < 3"
            , "x.-1"
            , "x.foo"
            , "f(,)"
            , "(1,,)"
            , "let in end"
            , "1 + lam(x: Int) => x"
            , "$x"
            , "'a"
            , "\"bad\\q\""
            , "\"unterminated"
            , "\"line\nbreak\""
            , "x y"
            , "1 %"
            , "% 2"
            , "1 %% 2"
            , "@if"
            ] $ \source ->
        assertParseError ("the expression " ++ show source ++ " is rejected")
          (pe source)

  , testCase "invalid declarations and types" $ do
      forM_ [ "val x = 1 val y = 2"
            , "dec x: Int dec y: Int"
            , "imp{a,a} f[a] = 1"
            , "dec f[a,a]: a"
            , "imp @f = 1"
            , "imp f{a}[a] = 1"
            , "dec f[a]: a[Int]"
            , "dec f[a]: a[]"
            , "local val x = 1 end"
            , ";"
            , "val x = 1;;"
            ] $ \source ->
        assertParseError ("the declaration " ++ show source ++ " is rejected")
          (pw source)
      forM_ ["int int", "(int,,)", "list[,]", "int ->"] $ \source ->
        assertParseError ("the type " ++ show source ++ " is rejected")
          (pt source)
      -- srcgen2 rejects imp{} f = 1 (empty braces); this grammar reads the
      -- empty parameter list as no parameters at all.
      assertParses "empty braces are accepted here"
        (DProgram [DTDec (TDecl ("f", [], SCon "Int" [])), DImpl (TImpl ("f", [], DInt 1, [], 0))]
                  (DInt 0))
        (pw "dec f: Int; imp{} f = 1\n0")

  , testCase "errors carry a position" $
      case pw "val x = 1;\nval y = $bad" of
        Left err -> assertEqual "the error is on the second line" 2
                      (sourceLine (errorPos err))
        Right _ -> ioError (userError "expected a parse error on line two")
  ]
  where
    lamBody (TImpl (_, _, body, _, _)) = body
    opName (DOp2 name _ _) = name
    opName _ = error "not a binary operator"
