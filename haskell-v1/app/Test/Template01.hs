-- | Port of python-v1\/TEST\/test01_2template.py: programs built from templates,
-- resolved and then interpreted.
module Test.Template01 (tests) where

import Control.Monad (forM_)

import Ast
import Interpret
import Template
import Test.Harness
import Test.Template.Helpers

-- | The template twice@(a -> a), b := f(f(x)).
twiceImpl :: DDecl
twiceImpl =
  let a = SVar "a"
  in DImpl "twice" (DApp (DVar "f") (DApp (DVar "f") (DVar "x")))
                  [("a", SFun a a), ("b", SVar "b")]

-- | The template fact@a := fix f(n). if n == 0 then 1 else n * f(n-1).
factImpl :: DDecl
factImpl =
  let a = SVar "a"
  in DImpl "fact"
       (DFix "f" ("n", a)
          (DIf (DOp2 "==" (DVar "n") (DInt 0))
               (DInt 1)
               (DOp2 "*"
                 (DVar "n")
                 (DApp (DVar "f") (DOp2 "-" (DVar "n") (DInt 1)))))
          (SFun a a))
       [("a", a)]

-- | The template inc3@a := inc@a + inc@a, with two nested uses.
inc3Impl :: DDecl
inc3Impl =
  DImpl "inc3"
    (DOp2 "+" (DTapp "inc" [SVar "a"]) (DTapp "inc" [SVar "a"]))
    [("a", SVar "a")]

fac :: Int -> Int
fac n = product [1 .. n]

tests :: [TestCase]
tests =
  [ testCase "pair and swap evaluate after replacement" $ do
      ctx <- implCtx [pairImpl, swapImpl]
      let text = con "Str"
          -- let x = 1; y = "hello"; p = pair@[Nat,Str]
          -- in swap@[Arr(Nat,Str)] end
          program =
            DLet
              [ DBind "x" (DInt 1)
              , DBind "y" (DStr "hello")
              , DBind "p" (DTapp "pair" [con "Nat", text])
              ]
              (Just (DTapp "swap" [arr (con "Nat") text]))
      result <- resolved program ctx
      assertValue "the swapped pair evaluates"
        (DValTupl [DValStr "hello", DValInt 1])
        (d2expEvaluateNil result)

  , testCase "twice applies a function twice" $ do
      ctx <- implCtx [twiceImpl]
      forM_ [0, 1, 5, 20 :: Int] $ \n -> do
        -- let f = lam y. y + 1; x = n in twice@[Nat -> Nat, Nat] end
        let program =
              DLet
                [ DBind "f" (DLam ("y", con "Nat") (DOp1 "+1" (DVar "y")))
                , DBind "x" (DInt n)
                ]
                (Just
                  (DTapp "twice"
                    [SFun (con "Nat") (con "Nat"), con "Nat"]))
        result <- resolved program ctx
        assertValue ("twice at " ++ show n) (DValInt (n + 2)) $
          d2expEvaluateNil result

  , testCase "function type argument must match" $ do
      ctx <- implCtx [twiceImpl]
      let use = DTapp "twice" [SFun (con "Nat") (con "Bool"), con "Nat"]
      assertLeft "resolving twice@[Nat -> Bool, Nat] fails" $
        templateExpand use ctx

  , testCase "template use inside a template is expanded" $ do
      ctx <- implCtx [incImpl, inc2Impl, inc3Impl]
      -- let n = n in inc2@[Nat] end, or inc3@[Nat] end, whose body uses inc
      -- twice
      forM_ [("inc2", 1), ("inc3", 2)] $ \(name, times) ->
        forM_ [0, 4, 10 :: Int] $ \n -> do
          let program = DLet [DBind "n" (DInt n)]
                             (Just (DTapp name [con "Nat"]))
          result <- resolved program ctx
          assertValue (name ++ " at " ++ show n) (DValInt (times * (n + 1))) $
            d2expEvaluateNil result

  , testCase "annotations are substituted in a recursive template" $ do
      ctx <- implCtx [factImpl]
      let nat = con "Nat"
          expected =
            DApp
              (DFix "f" ("n", nat)
                 (DIf (DOp2 "==" (DVar "n") (DInt 0))
                      (DInt 1)
                      (DOp2 "*"
                        (DVar "n")
                        (DApp (DVar "f") (DOp2 "-" (DVar "n") (DInt 1)))))
                 (SFun nat nat))
              (DInt 5)
      assertEqual "fact@Nat rewrites every annotation" expected =<<
        resolved (DApp (DTapp "fact" [nat]) (DInt 5)) ctx

  , testCase "factorial" $ do
      ctx <- implCtx [factImpl]
      forM_ [0, 1, 2, 5, 10 :: Int] $ \n -> do
        let use = DApp (DTapp "fact" [con "Nat"]) (DInt n)
        result <- resolved use ctx
        assertValue ("fact at " ++ show n) (DValInt (fac n)) $
          d2expEvaluateNil result
  ]