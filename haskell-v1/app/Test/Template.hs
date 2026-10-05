-- | Choosing implementations and expanding template uses, following
-- python-v1\/TEST\/test00_2template.py and TEST\/test01_2template.py: first the
-- choice an expansion makes, then the expansion itself, then the programs that
-- come out of it.
module Test.Template (tests) where

import Control.Monad (forM_)

import Data.List (nub)

import qualified Data.Map.Lazy as M

import Ast
import Interpret
import Parse (parseWhole)
import Template
import Test.Harness


-- | A type constructor without arguments, such as Nat or Bool.
con :: SName -> SExp
con name = SCon name []

-- | Build Arr(arg1, arg2).
arr :: SExp -> SExp -> SExp
arr arg1 arg2 = SCon "Arr" [arg1, arg2]

-- | An implementation: what it expands to, the argument types it takes, the
-- type of that, and a tag.  The tag is written out rather than counted up
-- because two implementations are the same exactly when their tags are.
timpl :: TName -> DExp -> [SExp] -> SExp -> Int -> DDecl
timpl name body pats ret tag = DImpl (TImpl (name, body, pats, ret, tag))

-- | zero@Nat := 0, with a concrete parameter type.
zeroImpl :: DDecl
zeroImpl = timpl "zero" (DInt 0) [con "Nat"] (con "Nat") 0

-- | id@a := lam x. x, with a variable parameter type.
idImpl :: DDecl
idImpl = timpl "id" (DLam ("x", SVar "a") (DVar "x")) [SVar "a"]
                   (SFun (SVar "a") (SVar "a")) 1

-- | pair@a,b := (x, y), which captures x and y.
pairImpl :: DDecl
pairImpl = timpl "pair" (DTuple [DVar "x", DVar "y"]) [SVar "a", SVar "b"]
                     (STuple [SVar "a", SVar "b"]) 2

-- | swap@a := (p.1, p.0), which captures p.
swapImpl :: DDecl
swapImpl = timpl "swap" (DTuple [DProj 1 (DVar "p"), DProj 0 (DVar "p")])
                     [SVar "a"] (STuple [SVar "a", SVar "a"]) 3

-- | inc@Nat := n + 1, which captures n.
incImpl :: DDecl
incImpl = timpl "inc" (DOp1 "+1" (DVar "n")) [con "Nat"] (con "Nat") 4

-- | inc2@a := inc@a, which uses another template.
inc2Impl :: DDecl
inc2Impl = timpl "inc2" (DTapp "inc" [SVar "a"]) [SVar "a"] (SVar "a") 5

-- | inc3@a := inc@a + inc@a, with two nested uses.
inc3Impl :: DDecl
inc3Impl = timpl "inc3"
  (DOp2 "+" (DTapp "inc" [SVar "a"]) (DTapp "inc" [SVar "a"]))
  [SVar "a"] (con "Nat") 6

-- | pick@a, implemented once for every type and once for Nat alone.
genericPick, concretePick :: DDecl
genericPick = timpl "pick" (DVar "generic") [SVar "a"] (SVar "a") 7
concretePick = timpl "pick" (DVar "nat") [con "Nat"] (con "Nat") 8

-- | annot@a := lam x. fix f(n). if n == 0 then x else f(n - 1), whose
-- annotations all mention the parameter.
annotBody :: SExp -> DExp
annotBody a =
  DLam ("x", a)
    (DFix "f" ("n", a)
       (DIf (DOp2 "==" (DVar "n") (DInt 0))
            (DVar "x")
            (DApp (DVar "f") (DOp2 "-" (DVar "n") (DInt 1))))
       (SFun a a))

annotImpl :: DDecl
annotImpl = timpl "annot" (annotBody (SVar "a")) [SVar "a"]
  (SFun (SVar "a") (SFun (SVar "a") (SVar "a"))) 9

-- | keep@a := lam x. lam y. x, where b stands for nothing.
keepImpl :: DDecl
keepImpl = timpl "keep" (DLam ("x", SVar "a") (DLam ("y", SVar "b") (DVar "x")))
                    [SVar "a"] (SFun (SVar "a") (SFun (SVar "b") (SVar "a"))) 10

-- | fact@a := fix f(n). if n == 0 then 1 else n * f(n - 1).
factImpl :: DDecl
factImpl = timpl "fact"
  (DFix "f" ("n", SVar "a")
     (DIf (DOp2 "==" (DVar "n") (DInt 0))
          (DInt 1)
          (DOp2 "*" (DVar "n")
                    (DApp (DVar "f") (DOp2 "-" (DVar "n") (DInt 1)))))
     (SFun (SVar "a") (SVar "a")))
  [SVar "a"] (SFun (SVar "a") (SVar "a")) 11

factorial :: Int -> Int
factorial n = product [1 .. n]


{- | Collect implementations in declaration order, so that the newest one ends up
nearest the front.

'insertDecl' extends the environment for the action it wraps rather than
replacing it, so the declarations have to be nested one inside the next for each
to be visible to the ones after it, and the context is read out from the
innermost action.  It also will not register an implementation whose template has
not been declared, so a declaration is written here for every name the list does
not declare itself; only the number of argument names matters, since
'chooseImpl' matches the implementation's argument types against them
positionally.
-}
implCtx :: [DDecl] -> IO TCtx
implCtx decls =
  unwrap "collecting implementations" $
    foldr (\decl act -> insertDecl decl act) (gets (\(_, x, _) -> x))
          (tdecls ++ decls)
  where
    declaredNames = [n | DTDec (TDecl (n, _, _)) <- decls]
    tdecls = nub
      [ DTDec (TDecl (n, argNames (length pats), ret))
      | DImpl (TImpl (n, _, pats, ret, _)) <- decls
      , n `notElem` declaredNames
      ]

argNames :: Int -> [SName]
argNames count = take count ["a", "b", "c"]

-- | The implementations recorded for a template name, newest first.
implDecls :: TCtx -> TName -> [DDecl]
implDecls tctx name = map DImpl (M.findWithDefault [] name (impls tctx))

{- | Run an action with a collected template context in hand.  'runM' always
starts from 'initEnv', so a test that has collected declarations supplies its
own here and the environment the action leaves behind is dropped, as 'runM'
drops it.
-}
withCtx :: TCtx -> M a -> M a
withCtx tctx act = TM $ \s _ ->
  case runTM act () (SCtx M.empty, tctx, []) of
    Left err    -> Left err
    Right (_, a) -> Right (s, a)

-- | 'chooseImpl' answers with the body it chose and the tag it came from.
chosen :: String -> TName -> [SExp] -> TCtx -> IO (DExp, Int)
chosen label name args tctx =
  unwrap label (withCtx tctx (chooseImpl name args))

-- | Replace every template use in the expression.  A use that does not resolve
-- fails the test here rather than being left in the result.
resolved :: DExp -> TCtx -> IO DExp
resolved dexp tctx =
  unwrap "resolving template uses" (withCtx tctx (templateExpand dexp))

-- | Every template use in the expression, in traversal order.
templateUses :: DExp -> [DExp]
templateUses = go
  where
    go e@(DTapp _ _) = [e]
    go (DOp1 _ a) = go a
    go (DOp2 _ a b) = go a ++ go b
    go (DLam _ b) = go b
    go (DFix _ _ b _) = go b
    go (DApp a b) = go a ++ go b
    go (DIf a b c) = go a ++ go b ++ go c
    go (DTuple ss) = concatMap go ss
    go (DProj _ a) = go a
    go (DLet ds body) = concatMap goDecl ds ++ maybe [] go body
    go _ = []

    goDecl (DBind _ e) = go e
    goDecl (DImpl (TImpl (_, e, _, _, _))) = go e
    goDecl _ = []

-- | Parse a program, expand its templates, and evaluate what is left.
runProgram :: [String] -> M DVal
runProgram source = do
  program <- case parseWhole (unlines source) of
    Left err -> gErr (show err)
    Right p  -> pure p
  expanded <- templateExpand program
  case expanded of
    DProgram binds body -> d2expLet [] binds (Just body)


tests :: [TestCase]
tests =
  [ testCase "collecting keeps only implementations, newest first" $ do
      tctx <- implCtx [DBind "x" (DInt 0), zeroImpl, idImpl]
      assertEqual "the bind records no implementation" []
        (implDecls tctx "x")
      assertEqual "the newer implementation is at the front" [idImpl]
        (implDecls tctx "id")
      assertEqual "the older implementation keeps its own name" [zeroImpl]
        (implDecls tctx "zero")

  , testCase "an implementation is chosen for a matching type" $ do
      tctx <- implCtx [zeroImpl]
      assertEqual "zero@Nat picks the body it was declared with" (DInt 0, 0)
        =<< chosen "choosing zero@Nat" "zero" [con "Nat"] tctx
      assertEqual "zero@Nat resolves to that body" (DInt 0)
        =<< resolved (DTapp "zero" [con "Nat"]) tctx
      assertLeft "zero@Bool does not match" $
        withCtx tctx (chooseImpl "zero" [con "Bool"])

  , testCase "an implementation over a variable matches any type" $ do
      tctx <- implCtx [idImpl]
      forM_ [con "Nat", arr (con "Nat") (con "Bool")] $ \s2t -> do
        assertEqual ("id@" ++ show s2t)
          (DApp (DLam ("x", s2t) (DVar "x")) (DInt 5))
          =<< resolved (DApp (DTapp "id" [s2t]) (DInt 5)) tctx

  , testCase "the newest implementation for a name wins" $ do
      tctx <- implCtx [genericPick, concretePick]
      assertEqual "pick@Nat takes the concrete implementation" (DVar "nat", 8)
        =<< chosen "choosing pick@Nat" "pick" [con "Nat"] tctx
      newestGeneric <- implCtx [concretePick, genericPick]
      forM_ [con "Nat", con "Bool"] $ \s2t ->
        assertEqual ("pick@" ++ show s2t ++ " takes the generic one")
          (DVar "generic", 7)
          =<< chosen "choosing pick" "pick" [s2t] newestGeneric

  , testCase "an implementation that does not match falls through" $ do
      tctx <- implCtx [genericPick, concretePick]
      assertEqual "pick@Nat takes the concrete implementation" (DVar "nat", 8)
        =<< chosen "choosing pick@Nat" "pick" [con "Nat"] tctx
      assertEqual "pick@Bool falls back to the older generic implementation"
        (DVar "generic", 7)
        =<< chosen "choosing pick@Bool" "pick" [con "Bool"] tctx

  , testCase "a use of an unknown template is unresolved" $ do
      tctx <- implCtx [idImpl]
      assertLeft "no implementation is found for the name" $
        withCtx tctx (chooseImpl "missing" [con "Nat"])
      assertLeft "expanding an unknown use fails" $
        withCtx tctx (templateExpand (DTapp "missing" [con "Nat"]))

  , testCase "expansion substitutes into the annotations of the body" $ do
      tctx <- implCtx [annotImpl]
      forM_ [con "Nat", arr (con "Nat") (con "Bool")] $ \s2t ->
        assertEqual ("annot@" ++ show s2t) (annotBody s2t)
          =<< resolved (DTapp "annot" [s2t]) tctx

  , testCase "a type variable nothing binds is left alone" $ do
      tctx <- implCtx [keepImpl]
      assertEqual "the unbound b survives"
        (DLam ("x", con "Nat") (DLam ("y", SVar "b") (DVar "x")))
        =<< resolved (DTapp "keep" [con "Nat"]) tctx

  , testCase "a use inside a template is expanded too" $ do
      tctx <- implCtx [incImpl, inc2Impl]
      let use = DApp (DTapp "inc2" [con "Nat"]) (DVar "n")
      result <- resolved use tctx
      assertEqual "no template uses are left" [] (templateUses result)
      assertEqual "inc2 expands through inc"
        (DApp (DOp1 "+1" (DVar "n")) (DVar "n")) result

  , testCase "an implementation declared in a let reaches its own body" $ do
      let nat = con "Nat"
          -- let tdecl inc<'a> Nat ; impl inc<Nat> n + 1
          -- in inc<Nat>(1) end
          program = DLet
            [ DTDec (TDecl ("inc", ["a"], nat))
            , DImpl (TImpl ("inc", (DOp1 "+1" (DVar "n")), [nat], nat, 12))
            ]
            (Just (DApp (DTapp "inc" [nat]) (DInt 1)))
      result <- unwrap "expanding the let" (templateExpand program)
      assertEqual "the use in the body was replaced" []
        (templateUses result)

  , testCase "an expression with no template uses is left alone" $ do
      let expression = DOp2 "+" (DVar "n") (DInt 1)
      assertEqual "an unchanged expression is rebuilt as itself" expression
        =<< resolved expression emptyTCtx

  , testCase "a resolved program evaluates" $ do
      tctx <- implCtx [pairImpl, swapImpl]
      let text = con "Str"
          -- let x = 1; y = "hello"; p = pair@[Nat,Str]
          -- in swap@[Arr(Nat,Str)] end
          program = DLet
            [ DBind "x" (DInt 1)
            , DBind "y" (DStr "hello")
            , DBind "p" (DTapp "pair" [con "Nat", text])
            ]
            (Just (DTapp "swap" [arr (con "Nat") text]))
      result <- resolved program tctx
      assertValue "the swapped pair evaluates"
        (DValTupl [DValStr "hello", DValInt 1])
        (d2expEvaluateNil result)

  , testCase "a recursive template evaluates" $ do
      tctx <- implCtx [factImpl]
      forM_ [0, 1, 2, 5, 10 :: Int] $ \n -> do
        result <- resolved (DApp (DTapp "fact" [con "Nat"]) (DInt n)) tctx
        assertValue ("fact " ++ show n) (DValInt (factorial n))
          (d2expEvaluateNil result)

  , testCase "nested templates evaluate" $ do
      tctx <- implCtx [incImpl, inc2Impl, inc3Impl]
      -- let n = n in inc2@[Nat] end, or inc3@[Nat] end, whose body uses inc
      -- twice
      forM_ [("inc2", 1), ("inc3", 2)] $ \(name, times) ->
        forM_ [0, 4, 10 :: Int] $ \n -> do
          let program = DLet [DBind "n" (DInt n)]
                         (Just (DTapp name [con "Nat"]))
          result <- resolved program tctx
          assertValue (name ++ " at " ++ show n) (DValInt (times * (n + 1)))
            (d2expEvaluateNil result)

  , testCase "a program with a template parses, expands and evaluates" $
      assertValue "twice@Nat applied to 20" (DValInt 21) $
        runProgram
          [ "tdecl twice<'a> ('a, 'a) -> 'a ;"
          , "inc = lam (n : Nat) n + 1 ;"
          , "impl twice<Nat> inc : Nat -> Nat ;"
          , "twice<Nat>(20)"
          ]
  ]
