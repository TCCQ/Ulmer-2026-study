-- | Plumbing shared by the template suites, mirroring the helpers at the top
-- of python's TEST\/test00_2template.py and TEST\/test01_2template.py.
module Test.Template.Helpers where

import Control.Monad (foldM)

import qualified Data.Map.Lazy as M

import Ast
import Template
import Test.Harness (unwrap)

{- | python's D2Cimpl keeps the universally quantified variable names and the
argument types in two separate lists, while the haskell DImpl pairs each name
with the type of the argument it introduces.  Where python supplies no name the
pairing is inert: 'chooseImpl' merges only the contexts that 'sMatch' produces
and never the names, so a placeholder name stands in for the missing one.  The
implementations below are written with the name python would have used where it
has one, and with a placeholder where it does not.
-}

-- | A type constructor without arguments, such as Nat or Bool.
con :: SName -> SExp
con name = SCon name []

-- | Build Arr(arg1, arg2).
arr :: SExp -> SExp -> SExp
arr arg1 arg2 = SCon "Arr" [arg1, arg2]

{- | Collect implementations in order, so that the newest declaration ends up
nearest the front.  Only an implementation contributes; every other kind of
declaration leaves the context untouched, as python's s2tmp_collect does.
-}
implCtx :: [DDecl] -> IO SCtx
implCtx decls =
  unwrap "collecting implementations" (foldM step substEmpty decls)
  where
    step ctx (DImpl n body args) = insertImpl n body args ctx
    step ctx _                  = pure ctx

-- | The implementations recorded for a template name, newest first.
tctxDecls :: SCtx -> TName -> [DDecl]
tctxDecls (SCtx _ tctx) n = M.findWithDefault [] n tctx

{- | Replace every template use in the expression.  python returns a fnoptn and
asserts that a result was produced, so a failure to resolve fails the test here
instead.
-}
resolved :: DExp -> SCtx -> IO DExp
resolved dexp ctx = unwrap "resolving template uses" (templateExpand dexp ctx)

-- | Collect every template use in the expression, in traversal order.
templateUses :: DExp -> [DExp]
templateUses = go
  where
    go e@(DTapp _ _) = [e]
    go (DOp1 _ a) = go a
    go (DOp2 _ a b) = go a ++ go b
    go (DLam _ b) = go b
    go (DFix _ _ b _) = go b
    go (DApp a b) = go a ++ go b
    go (DIf c a b) = go c ++ go a ++ go b
    go (DTuple ss) = concatMap go ss
    go (DProj _ a) = go a
    go (DLet ds body) = declUses ds ++ maybe [] go body
    go _ = []

    declUses = concatMap one
    one (DBind _ e) = go e
    one (DImpl _ e _) = go e
    one (DLocal head' body) = declUses head' ++ declUses body

-- | The template zero@Nat := 0, with a concrete parameter type.
zeroImpl :: DDecl
zeroImpl = DImpl "zero" (DInt 0) [("a", con "Nat")]

-- | The template id@a := lam x. x, with a variable parameter type.
identityImpl :: DDecl
identityImpl = DImpl "id" (DLam ("x", SVar "a") (DVar "x")) [("a", SVar "a")]

-- | The template pair@a,b := (x, y), which captures x and y.
pairImpl :: DDecl
pairImpl = DImpl "pair" (DTuple [DVar "x", DVar "y"])
                   [("a", SVar "a"), ("b", SVar "b")]

-- | The template swap@a := (p.1, p.0), which captures p.
swapImpl :: DDecl
swapImpl =
  DImpl "swap" (DTuple [DProj 1 (DVar "p"), DProj 0 (DVar "p")])
               [("a", SVar "a")]

-- | The template inc@Nat := n + 1, which captures n.
incImpl :: DDecl
incImpl = DImpl "inc" (DOp1 "+1" (DVar "n")) [("a", con "Nat")]

-- | The template inc2@a := inc@a(n), which uses another template.
inc2Impl :: DDecl
inc2Impl = DImpl "inc2" (DTapp "inc" [SVar "a"]) [("a", SVar "a")]