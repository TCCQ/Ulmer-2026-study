-- | Choosing implementations and expanding template uses, following
-- python-v1\/TEST\/test00_2template.py and TEST\/test01_2template.py: first the
-- choice an expansion makes, then the expansion itself, then the programs that
-- come out of it.
module Test.Template (tests) where

import Control.Monad (forM_)

import Data.List (nub)

import qualified Data.Map.Lazy as M

import Ast
import Bind (bindProgram)
import Interpret
import Parse (parseWhole)
import Template
import Test.Harness


-- | A type constructor without arguments, such as Nat or Bool.
con :: String -> SExp Name
con name = SCon (testName name) []

-- | Build Arr(arg1, arg2).
arr :: SExp Name -> SExp Name -> SExp Name
arr arg1 arg2 = SCon (testName "Arr") [arg1, arg2]

-- | An implementation: what it expands to, the argument types it takes, and a
-- tag.  The tag is written out rather than counted up because two
-- implementations are the same exactly when their tags are.  The result type is
-- not stored: the checker derives it from the declaration.
timpl :: String -> DExp Name -> [SExp Name] -> Int -> DDecl Name
timpl name body pats tag = DImpl (TImpl (testName name, [], body, pats, tag))

-- | zero@Nat := 0, with a concrete parameter type.
zeroImpl :: DDecl Name
zeroImpl = timpl "zero" (DInt 0) [con "Nat"] 0

-- | id@a := lam x. x, with a variable parameter type.
idImpl :: DDecl Name
idImpl = timpl "id" (DLam (testName "x", SVar (testName "a")) (DVar (testName "x")))
                   [SVar (testName "a")] 1

-- | pair@a,b := (x, y), which captures x and y.
pairImpl :: DDecl Name
pairImpl = timpl "pair"
  (DTuple [DVar (testName "x"), DVar (testName "y")])
  [SVar (testName "a"), SVar (testName "b")] 2

-- | swap@a := (p.1, p.0), which captures p.
swapImpl :: DDecl Name
swapImpl = timpl "swap"
  (DTuple [DProj 1 (DVar (testName "p")), DProj 0 (DVar (testName "p"))])
  [SVar (testName "a")] 3

-- | inc@Nat := n + 1, which captures n.
incImpl :: DDecl Name
incImpl = timpl "inc"
  (DOp2 (testName "+") (DVar (testName "n")) (DInt 1)) [con "Nat"] 4

-- | inc2@a := inc@a, which uses another template.
inc2Impl :: DDecl Name
inc2Impl = timpl "inc2" (DTapp (testName "inc") [SVar (testName "a")])
               [SVar (testName "a")] 5

-- | inc3@a := inc@a + inc@a, with two nested uses.
inc3Impl :: DDecl Name
inc3Impl = timpl "inc3"
  (DOp2 (testName "+")
        (DTapp (testName "inc") [SVar (testName "a")])
        (DTapp (testName "inc") [SVar (testName "a")]))
  [SVar (testName "a")] 6

-- | pick@a, implemented once for every type and once for Nat alone.
genericPick, concretePick :: DDecl Name
genericPick = timpl "pick" (DVar (testName "generic")) [SVar (testName "a")] 7
concretePick = timpl "pick" (DVar (testName "nat")) [con "Nat"] 8

-- | annot@a := lam x. fix f(n). if n == 0 then x else f(n - 1), whose
-- annotations all mention the parameter.
annotBody :: SExp Name -> DExp Name
annotBody a =
  DLam (testName "x", a)
    (DFix (testName "f") (testName "n", a)
       (DIf (DOp2 (testName "==") (DVar (testName "n")) (DInt 0))
            (DVar (testName "x"))
            (DApp (DVar (testName "f"))
                  (DOp2 (testName "-") (DVar (testName "n")) (DInt 1))))
       (SFun a a))

annotImpl :: DDecl Name
annotImpl = timpl "annot" (annotBody (SVar (testName "a")))
                          [SVar (testName "a")] 9

-- | keep@a := lam x. lam y. x, where b stands for nothing.
keepImpl :: DDecl Name
keepImpl = timpl "keep"
  (DLam (testName "x", SVar (testName "a"))
        (DLam (testName "y", SVar (testName "b")) (DVar (testName "x"))))
  [SVar (testName "a")] 10

-- | fact@a := fix f(n). if n == 0 then 1 else n * f(n - 1).
factImpl :: DDecl Name
factImpl = timpl "fact"
  (DFix (testName "f") (testName "n", SVar (testName "a"))
     (DIf (DOp2 (testName "==") (DVar (testName "n")) (DInt 0))
          (DInt 1)
          (DOp2 (testName "*") (DVar (testName "n"))
                    (DApp (DVar (testName "f"))
                          (DOp2 (testName "-") (DVar (testName "n")) (DInt 1)))))
     (SFun (SVar (testName "a")) (SVar (testName "a"))))
  [SVar (testName "a")] 11

selfImpl :: DDecl Name
selfImpl = timpl "self" (DTapp (testName "self") [con "Nat"]) [con "Nat"] 20

selfBase :: DDecl Name
selfBase = timpl "self" (DInt 1) [con "Nat"] 19

innerA :: DDecl Name
innerA = timpl "inner" (DInt 1) [con "Nat"] 30

innerB :: DDecl Name
innerB = timpl "inner" (DInt 2) [con "Nat"] 31

outerUse :: DDecl Name
outerUse = timpl "outer" (DTapp (testName "inner") [con "Nat"]) [con "Nat"] 32

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
implCtx :: [DDecl Name] -> IO (TCtx Name)
implCtx decls =
  unwrap "collecting implementations" $
    foldr (\decl act -> insertDecl decl act) (gets (\(_, x, _) -> x))
          (tdecls ++ decls)
  where
    declaredNames = [n | DTDec (TDecl (n, _, _)) <- decls]
    tdecls = nub
      [ DTDec (TDecl (n, argNames (length pats), anyType))
      | DImpl (TImpl (n, _, _, pats, _)) <- decls
      , n `notElem` declaredNames
      ]

argNames :: Int -> [Name]
argNames count = take count (map testName ["a", "b", "c"])

-- | The implementations recorded for a template name, newest first.
implDecls :: TCtx Name -> String -> [DDecl Name]
implDecls tctx name =
  map (DImpl . fst) (M.findWithDefault [] (testName name) (impls tctx))

{- | Run an action with a collected template context in hand.  'evalM' always
starts from 'initEnv', so a test that has collected declarations supplies its
own here and the environment the action leaves behind is dropped, as 'evalM'
drops it.
-}
withCtx :: TCtx Name -> M a -> M a
withCtx tctx act = TM $ \s _ ->
  case runTM act 0 (SCtx M.empty, tctx, []) of
    Left err    -> Left err
    Right (_, a) -> Right (s, a)

-- | 'chooseImpl' answers with the body it chose and the tag it came from.
chosen :: String -> String -> [SExp Name] -> TCtx Name -> IO (DExp Name, Int)
chosen label name args tctx =
  unwrap label (withCtx tctx (chooseImpl (testName name) args))

-- | Replace every template use in the expression.  A use that does not resolve
-- fails the test here rather than being left in the result.
resolved :: DExp Name -> TCtx Name -> IO (DExp Name)
resolved dexp tctx =
  unwrap "resolving template uses" (withCtx tctx (templateExpand dexp))

-- | Every template use in the expression, in traversal order.
templateUses :: DExp Name -> [DExp Name]
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
    goDecl (DImpl (TImpl (_, _, e, _, _))) = go e
    goDecl _ = []

-- | Parse a program, bind its names, expand its templates, and evaluate what is
-- left.
runProgram :: [String] -> M DVal
runProgram source = do
  program <- case parseWhole (unlines source) of
    Left err -> gErr (show err)
    Right p  -> case bindProgram p of
      Left err -> gErr (show err)
      Right (_, p') -> pure p'
  expanded <- templateExpand program
  case expanded of
    DProgram binds body -> d2expLet [] binds (Just body)


tests :: [TestCase]
tests =
  [ testCase "collecting keeps only implementations, newest first" $ do
      tctx <- implCtx [DBind (testName "x") (DInt 0), zeroImpl, idImpl]
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
        =<< resolved (DTapp (testName "zero") [con "Nat"]) tctx
      assertLeft "zero@Bool does not match" $
        withCtx tctx (chooseImpl (testName "zero") [con "Bool"])

  , testCase "an implementation over a variable matches any type" $ do
      tctx <- implCtx [idImpl]
      forM_ [con "Nat", arr (con "Nat") (con "Bool")] $ \s2t -> do
        assertEqual ("id@" ++ show s2t)
          (DApp (DLam (testName "x", s2t) (DVar (testName "x"))) (DInt 5))
          =<< resolved (DApp (DTapp (testName "id") [s2t]) (DInt 5)) tctx

  , testCase "the newest implementation for a name wins" $ do
      tctx <- implCtx [genericPick, concretePick]
      assertEqual "pick@Nat takes the concrete implementation"
        (DVar (testName "nat"), 8)
        =<< chosen "choosing pick@Nat" "pick" [con "Nat"] tctx
      newestGeneric <- implCtx [concretePick, genericPick]
      forM_ [con "Nat", con "Bool"] $ \s2t ->
        assertEqual ("pick@" ++ show s2t ++ " takes the generic one")
          (DVar (testName "generic"), 7)
          =<< chosen "choosing pick" "pick" [s2t] newestGeneric

  , testCase "an implementation that does not match falls through" $ do
      tctx <- implCtx [genericPick, concretePick]
      assertEqual "pick@Nat takes the concrete implementation"
        (DVar (testName "nat"), 8)
        =<< chosen "choosing pick@Nat" "pick" [con "Nat"] tctx
      assertEqual "pick@Bool falls back to the older generic implementation"
        (DVar (testName "generic"), 7)
        =<< chosen "choosing pick@Bool" "pick" [con "Bool"] tctx

  , testCase "a use of an unknown template is unresolved" $ do
      tctx <- implCtx [idImpl]
      assertLeft "no implementation is found for the name" $
        withCtx tctx (chooseImpl (testName "missing") [con "Nat"])
      assertLeft "expanding an unknown use fails" $
        withCtx tctx (templateExpand (DTapp (testName "missing") [con "Nat"]))

  , testCase "expansion substitutes into the annotations of the body" $ do
      tctx <- implCtx [annotImpl]
      forM_ [con "Nat", arr (con "Nat") (con "Bool")] $ \s2t ->
        assertEqual ("annot@" ++ show s2t) (annotBody s2t)
          =<< resolved (DTapp (testName "annot") [s2t]) tctx

  , testCase "a type variable nothing binds is left alone" $ do
      tctx <- implCtx [keepImpl]
      assertEqual "the unbound b survives"
        (DLam (testName "x", con "Nat")
              (DLam (testName "y", SVar (testName "b")) (DVar (testName "x"))))
        =<< resolved (DTapp (testName "keep") [con "Nat"]) tctx

  , testCase "a use inside a template is expanded too" $ do
      tctx <- implCtx [incImpl, inc2Impl]
      let use = DApp (DTapp (testName "inc2") [con "Nat"]) (DVar (testName "n"))
      result <- resolved use tctx
      assertEqual "no template uses are left" [] (templateUses result)
      assertEqual "inc2 expands through inc"
        (DApp (DOp2 (testName "+") (DVar (testName "n")) (DInt 1))
              (DVar (testName "n"))) result

  , testCase "an implementation declared in a let reaches its own body" $ do
      let nat = con "Nat"
          -- let tdecl inc<'a> Nat ; impl inc<Nat> n + 1
          -- in inc<Nat>(1) end
          program = DLet
            [ DTDec (TDecl (testName "inc", [testName "a"], nat))
            , DImpl (TImpl (testName "inc", []
                           , DOp2 (testName "+") (DVar (testName "n")) (DInt 1)
                           , [nat], 12))
            ]
            (Just (DApp (DTapp (testName "inc") [nat]) (DInt 1)))
      result <- unwrap "expanding the let" (templateExpand program)
      assertEqual "the use in the body was replaced" []
        (templateUses result)

  , testCase "an expression with no template uses is left alone" $ do
      let expression =
            DOp2 (testName "+") (DVar (testName "n")) (DInt 1)
      assertEqual "an unchanged expression is rebuilt as itself" expression
        =<< resolved expression emptyTCtx

  , testCase "a resolved program evaluates" $ do
      tctx <- implCtx [pairImpl, swapImpl]
      let text = con "Str"
          -- let x = 1; y = "hello"; p = pair@[Nat,Str]
          -- in swap@[Arr(Nat,Str)] end
          program = DLet
            [ DBind (testName "x") (DInt 1)
            , DBind (testName "y") (DStr "hello")
            , DBind (testName "p") (DTapp (testName "pair") [con "Nat", text])
            ]
            (Just (DTapp (testName "swap") [arr (con "Nat") text]))
      result <- resolved program tctx
      assertValue "the swapped pair evaluates"
        (DValTupl [DValStr "hello", DValInt 1])
        (d2expEvaluateNil result)

  , testCase "a recursive template evaluates" $ do
      tctx <- implCtx [factImpl]
      forM_ [0, 1, 2, 5, 10 :: Int] $ \n -> do
        result <- resolved (DApp (DTapp (testName "fact") [con "Nat"]) (DInt n)) tctx
        assertValue ("fact " ++ show n) (DValInt (factorial n))
          (d2expEvaluateNil result)

  , testCase "nested templates evaluate" $ do
      tctx <- implCtx [incImpl, inc2Impl, inc3Impl]
      -- let n = n in inc2@[Nat] end, or inc3@[Nat] end, whose body uses inc
      -- twice
      forM_ [("inc2", 1), ("inc3", 2)] $ \(name, times) ->
        forM_ [0, 4, 10 :: Int] $ \n -> do
          let program = DLet [DBind (testName "n") (DInt n)]
                         (Just (DTapp (testName name) [con "Nat"]))
          result <- resolved program tctx
          assertValue (name ++ " at " ++ show n) (DValInt (times * (n + 1)))
            (d2expEvaluateNil result)

  , testCase "a program with a template parses, expands and evaluates" $
      assertValue "twice@Int applied to 20" (DValInt 21) $
        runProgram
          [ "dec twice[a]: (a, a) -> a;"
          , "val inc = lam (n : Int) => n + 1;"
          , "imp twice[Int] = inc;"
          , "@twice[Int](20)"
          ]
  , testCase "a self-recursive implementation is blocked by the blacklist" $ do
      tctx <- implCtx [selfImpl]
      assertLeft "expanding the self use fails" $
        withCtx tctx (templateExpand (DTapp (testName "self") [con "Nat"]))

  , testCase "the blacklist only blocks the implementation that was chosen" $ do
      tctx <- implCtx [selfBase, selfImpl]
      assertEqual "the older implementation fills the recursive use" (DInt 1)
        =<< resolved (DTapp (testName "self") [con "Nat"]) tctx
  ]
