-- | Port of python-v1\/TEST\/test00_2template.py: choosing implementations and
-- expanding template uses.
module Test.Template00 (tests) where

import Control.Monad (forM_)

import Ast
import Template
import Test.Harness
import Test.Template.Helpers

{- | Two implementations of the same template, one generic and one concrete.
python writes both with an empty variable list and takes the parameter types
alone.
-}
pickGeneric, pickConcrete :: DDecl
pickGeneric = DImpl "pick" (DVar "generic") [("a", SVar "a")]
pickConcrete = DImpl "pick" (DVar "nat") [("a", con "Nat")]

{- | The template annot@a := lam x. fix f(n). if n == 0 then x else f(n - 1),
whose annotations all mention the parameter.
-}
annotBody :: SExp -> DExp
annotBody a =
  DLam ("x", a)
    (DFix "f" ("n", a)
       (DIf (DOp2 "==" (DVar "n") (DInt 0))
            (DVar "x")
            (DApp (DVar "f") (DOp2 "-" (DVar "n") (DInt 1))))
       (SFun a a))

annotImpl :: DDecl
annotImpl = DImpl "annot" (annotBody (SVar "a")) [("a", SVar "a")]

tests :: [TestCase]
tests =
  [ testCase "collecting a bind leaves the context alone" $ do
      withBind <- implCtx [DBind "x" (DInt 0), zeroImpl]
      assertEqual "the bind records no implementation" []
        (tctxDecls withBind "x")
      assertEqual "the implementation is still collected" [zeroImpl]
        (tctxDecls withBind "zero")

  , testCase "collecting prepends the newest implementation" $ do
      ctx <- implCtx [zeroImpl, identityImpl]
      assertEqual "the newest implementation sits at the front of its name"
        [identityImpl] (tctxDecls ctx "id")
      assertEqual "the older implementation keeps its own name"
        [zeroImpl] (tctxDecls ctx "zero")
      -- python's context is a single list with the newest entry in front; the
      -- haskell context groups by name, so prepending shows up within a name.
      let newerZero = DImpl "zero" (DInt 1) [("a", con "Nat")]
      sameName <- implCtx [zeroImpl, newerZero]
      assertEqual "both declarations are kept, newest first"
        [newerZero, zeroImpl] (tctxDecls sameName "zero")

  , testCase "implementation for a concrete type is found" $ do
      ctx <- implCtx [zeroImpl]
      assertValue "chooseImpl picks the body" (DInt 0) $
        chooseImpl "zero" [con "Nat"] ctx
      assertEqual "zero@Nat resolves to its body" (DInt 0) =<<
        resolved (DTapp "zero" [con "Nat"]) ctx

  , testCase "implementation over a variable is found" $ do
      ctx <- implCtx [identityImpl]
      forM_ [con "Nat", con "Bool", arr (con "Nat") (con "Bool")] $ \s2t -> do
        let label = "id@" ++ show s2t
        assertEqual label
          (DApp (DLam ("x", s2t) (DVar "x")) (DInt 5))
          =<< resolved (DApp (DTapp "id" [s2t]) (DInt 5)) ctx

  , testCase "concrete implementation rejects another type" $ do
      ctx <- implCtx [zeroImpl]
      assertLeft "chooseImpl rejects zero@Bool" $
        chooseImpl "zero" [con "Bool"] ctx
      assertLeft "resolving zero@Bool fails" $
        templateExpand (DTapp "zero" [con "Bool"]) ctx

  , testCase "newest declaration for a name wins" $ do
      -- A use of pick@Nat picks the concrete implementation when it is
      -- declared last, and the generic one when the generic implementation
      -- is declared last.
      ctx <- implCtx [pickGeneric, pickConcrete]
      assertEqual "the concrete implementation is declared last" (DVar "nat")
        =<< resolved (DTapp "pick" [con "Nat"]) ctx
      newestGeneric <- implCtx [pickConcrete, pickGeneric]
      forM_ [con "Nat", con "Bool"] $ \s2t ->
        assertEqual ("the generic implementation at " ++ show s2t)
          (DVar "generic")
          =<< resolved (DTapp "pick" [s2t]) newestGeneric

  , testCase "implementation falls through to an older declaration" $ do
      -- The newest implementation for pick does not take Bool, so the older
      -- generic implementation is used instead.
      ctx <- implCtx [pickGeneric, pickConcrete]
      assertEqual "pick@Nat takes the concrete implementation" (DVar "nat")
        =<< resolved (DTapp "pick" [con "Nat"]) ctx
      assertEqual "pick@Bool falls through to the generic implementation"
        (DVar "generic")
        =<< resolved (DTapp "pick" [con "Bool"]) ctx

  , testCase "implementation is found past another declaration" $ do
      ctx <- implCtx [zeroImpl, identityImpl]
      assertEqual "zero@Nat resolves past id" (DInt 0)
        =<< resolved (DTapp "zero" [con "Nat"]) ctx

  , testCase "use of an unknown template is unresolved" $ do
      ctx <- implCtx [identityImpl]
      assertLeft "chooseImpl finds no implementation" $
        chooseImpl "missing" [con "Nat"] ctx
      assertLeft "resolving an unknown template fails" $
        templateExpand (DTapp "missing" [con "Nat"]) ctx

  , testCase "successful replacement leaves no template use" $ do
      ctx <- implCtx [pairImpl, swapImpl]
      let text = con "Str"
          -- let x = 1; y = "hello"; p = pair@[Nat,Str]
          -- in swap@[Arr(Nat,Str)](p) end
          program =
            DLet
              [ DBind "x" (DInt 1)
              , DBind "y" (DStr "hello")
              , DBind "p" (DTapp "pair" [con "Nat", text])
              ]
              (Just (DApp (DTapp "swap" [arr (con "Nat") text]) (DVar "p")))
          expected =
            DLet
              [ DBind "x" (DInt 1)
              , DBind "y" (DStr "hello")
              , DBind "p" (DTuple [DVar "x", DVar "y"])
              ]
              (Just
                (DApp
                  (DTuple [DProj 1 (DVar "p"), DProj 0 (DVar "p")])
                  (DVar "p")))
      result <- resolved program ctx
      assertEqual "no template uses remain" [] (templateUses result)
      assertEqual "the whole program is expanded" expected result

  , testCase "nested template uses are expanded" $ do
      ctx <- implCtx [incImpl, inc2Impl]
      let use = DApp (DTapp "inc2" [con "Nat"]) (DVar "n")
      result <- resolved use ctx
      assertEqual "no template uses remain" [] (templateUses result)
      assertEqual "inc2 expands through inc" (DApp (DOp1 "+1" (DVar "n")) (DVar "n")) result

  , testCase "substitution is applied to annotations in the body" $ do
      ctx <- implCtx [annotImpl]
      forM_ [con "Nat", arr (con "Nat") (con "Bool")] $ \s2t -> do
        assertEqual ("annot@" ++ show s2t) (annotBody s2t)
          =<< resolved (DTapp "annot" [s2t]) ctx

  , testCase "unbound type variables are kept" $ do
      -- keep@a := lam x. lam y. x, where b names no parameter of the
      -- template and so survives the substitution untouched.
      ctx <- implCtx
        [DImpl "keep"
          (DLam ("x", SVar "a") (DLam ("y", SVar "b") (DVar "x")))
          [("a", SVar "a")]]
      assertEqual "the unbound b survives"
        (DLam ("x", con "Nat") (DLam ("y", SVar "b") (DVar "x")))
        =<< resolved (DTapp "keep" [con "Nat"]) ctx

  , testCase "declarations in a let are expanded" $ do
      ctx <- implCtx [zeroImpl]
      {- let a = zero@[Nat]; b = f(0); unused = zero@[Nat] in b end, where the
      implementation body is expanded but its signature is not rewritten,
      because no substitution applies to a traversed implementation.
      -}
      let unused = DImpl "unused" (DTapp "zero" [con "Nat"]) [("a", SVar "z")]
          program =
            DLet
              [ DBind "a" (DTapp "zero" [con "Nat"])
              , DBind "b" (DApp (DVar "f") (DInt 0))
              , unused
              ]
              (Just (DVar "b"))
          expected =
            DLet
              [ DBind "a" (DInt 0)
              , DBind "b" (DApp (DVar "f") (DInt 0))
              , DImpl "unused" (DInt 0) [("a", SVar "z")]
              ]
              (Just (DVar "b"))
      result <- resolved program ctx
      assertEqual "no template uses remain" [] (templateUses result)
      assertEqual "the let declarations are expanded" expected result

  , testCase "expression without template uses is rebuilt" $ do
      -- python also asserts the result is not the same object, showing the
      -- walk rebuilt the tree; rebuilding a pure value is unobservable here.
      let expression = DOp2 "+" (DVar "n") (DInt 1)
      assertEqual "an unchanged expression is rebuilt as itself" expression
        =<< resolved expression substEmpty

  , testCase "unresolved use raises" $
      assertLeft "resolving an unknown template against an empty context fails" $
        templateExpand (DTapp "missing" [con "Nat"]) substEmpty
  ]