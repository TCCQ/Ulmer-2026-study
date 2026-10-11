{-# LANGUAGE FlexibleInstances #-}
module Ast.Pretty where

import Prelude hiding ((<>))

import Data.List (nub)

import Ast.Types
import Ast.Util (freeS)

import Text.PrettyPrint

class Pretty a where
  pp :: a -> Doc

instance Pretty String where
  pp = text

-- instance Pretty SName where
--   pp = text

instance Pretty SExp where
  pp (SCon n args) = pp n <> brackets (sep $ punctuate comma (pp <$> args))
  pp (SVar n) = pp n
  pp (SFun l r) = parens (pp l) <+> text "->" <+> pp r
  pp (STuple as) = parens $ sep $ punctuate comma $ pp <$> as

instance Pretty DExp where
  pp (DInt i) = int i
  pp (DBool b) = if b then text "true" else text "false"
  pp (DStr s) = text s
  pp (DOp1 s e) = text s <> parens (pp e)
  pp (DOp2 s l r) = parens (pp l) <+> pp s <+> parens (pp r)
  pp (DVar n) = pp n
  pp (DLam (a, at) b) =
    let line1 = text "lam" <+> parens ((pp a) <+> char ':' <+> pp at) <+> text "=>"
    in hang line1 2 (pp b)
  pp (DFix n (a, at) b tt) =
    let line1 = text "fix" <+> pp n <+> parens (pp a <+> char ':' <+> pp at) <+>
                char ':' <+> pp tt <+> text "=>"
    in hang line1 2 (pp b)
  pp (DApp l r) = pp l <> parens (pp r)
  pp (DIf c t f) =
    let line1 = text "if" <+> pp c
        line2 = hang (text "then") 2 (pp t)
        line3 = hang (text "else") 2 (pp f)
    in line1 $$ line2 $$ line3
  pp (DTuple as) = parens $ sep $ punctuate comma $ pp <$> as
  pp (DProj n e) = parens (pp e) <> char '.' <> int n
  pp (DLet binds body) =
    let header = text "let" $+$ nest 2 (sep $ punctuate semi (pp <$> binds))
    in case body of
         Nothing -> header $+$ text "end"
         Just b  -> header $+$ text "in" $+$ nest 2 (pp b) $+$ text "end"
  pp (DTapp n args) = char '@' <> pp n <> brackets (sep $ punctuate comma $ pp <$> args)

instance Pretty TDecl where
  pp (TDecl (n, as, r)) =
    text "dec" <+> pp n <> brackets (sep $ punctuate comma (pp <$> as)) <+> char ':' <+> pp r

instance Pretty TImpl where
  pp (TImpl (n, body, binds, _)) =
    let params = nub (concatMap freeS binds)
        braces' = if null params
          then empty
          else braces (sep $ punctuate comma (pp <$> params)) <> space
    in text "imp" <+> braces' <+> pp n <>
       brackets (sep $ punctuate comma $ pp <$> binds) <+> char '=' <+> pp body

instance Pretty DDecl where
  pp (DBind n b) = text "val" <+> pp n <+> char '=' <+> pp b
  pp (DTDec t) = pp t
  pp (DImpl t) = pp t
  pp (DLocal bs ds) =
    hang (text "local") 2 (vcat $ punctuate semi (map pp bs)) $+$
    hang (text "in") 2 ((vcat $ punctuate semi (map pp ds))) $+$
    text "end"

instance Pretty DProgram where
  pp (DProgram binds body) = (sep (punctuate semi $ pp <$> binds) <> semi) $+$ pp body
