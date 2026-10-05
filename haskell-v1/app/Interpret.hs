module Interpret where

import Prelude hiding ((<>))

import Ast

import Text.PrettyPrint

data DVal
  = DValInt Int
  | DValBtf Bool
  | DValStr String
  | DValTupl [DVal]
  | DValLam DEnv (DName, SExp) DExp
  | DValFix DEnv DName (DName, SExp) DExp SExp
  deriving (Show, Eq)

type DEnv = [(DName, DVal)]

instance Pretty DVal where
  pp (DValInt i) = int i
  pp (DValBtf b) = if b then text "true" else text "false"
  pp (DValStr s) = text s
  pp (DValTupl vs) = parens $ sep $ punctuate comma $ pp <$> vs
  pp (DValLam _ (a, at) b) =
    let line1 = text "lam" <+> parens ((pp a) <+> char ':' <+> pp at)
    in hang line1 2 (pp b)
  pp (DValFix _ nSelf (a, at) b ot) =
    let line1 = text "fix" <+> parens (pp nSelf <> comma <+> pp a <+> char ':' <+> pp at)
        line2 = pp b
    in hang (hang line1 4 line2) 2 (char ':' <+> pp ot)

dValNil :: DVal
dValNil = DValTupl []

d2expEvaluateNil :: DExp -> M DVal
d2expEvaluateNil dexp = d2expEvaluate dexp []

d2expEvaluate :: DExp -> DEnv -> M DVal
d2expEvaluate dexp denv = case dexp of
  DInt i -> pure (DValInt i)
  DBool b -> pure (DValBtf b)
  DStr s -> pure (DValStr s)
  DOp1 n a -> d2expOp1 denv n a
  DOp2 n a b -> d2expOp2 denv n a b
  DVar n -> case lookup n denv of
    Just v -> pure v
    Nothing -> gErr $ "Unbound variable " ++ n
  DLam nt body -> pure (DValLam denv nt body)
  DFix nSelf nt body ot -> pure (DValFix denv nSelf nt body ot)
  DApp f a -> d2expApp denv f a
  DIf c a b -> d2expIf denv c a b
  DTuple ss -> DValTupl <$> mapM (flip d2expEvaluate denv) ss
  DProj i e -> d2expProj denv i e
  DLet ds body -> d2expLet denv ds body
  DTapp e _ -> gErr $ "Template application " ++ show e ++
                       " cannot be interpreted"

d2expOp1 :: DEnv -> DName -> DExp -> M DVal
d2expOp1 denv n a = do
  va <- d2expEvaluate a denv
  case va of
    DValInt i -> case n of
      "+1" -> pure (DValInt (i + 1))
      "-1" -> pure (DValInt (i - 1))
      _ -> gErr $ "Unknown unary operator " ++ n
    _ -> gErr $ "Operator " ++ n ++ " expects an integer argument"

d2expOp2 :: DEnv -> DName -> DExp -> DExp -> M DVal
d2expOp2 denv n a b = do
  va <- d2expEvaluate a denv
  vb <- d2expEvaluate b denv
  case (va, vb) of
    (DValInt x, DValInt y) -> d2intOp2 n x y
    _ -> gErr $ "Operator " ++ n ++ " expects integer arguments"

d2intOp2 :: DName -> Int -> Int -> M DVal
d2intOp2 n x y = case n of
  "+" -> pure (DValInt (x + y))
  "-" -> pure (DValInt (x - y))
  "*" -> pure (DValInt (x * y))
  "<" -> pure (DValBtf (x < y))
  ">" -> pure (DValBtf (x > y))
  "<=" -> pure (DValBtf (x <= y))
  ">=" -> pure (DValBtf (x >= y))
  "==" -> pure (DValBtf (x == y))
  "!=" -> pure (DValBtf (x /= y))
  _ -> gErr $ "Unknown binary operator " ++ n

d2expApp :: DEnv -> DExp -> DExp -> M DVal
d2expApp denv f a = do
  vf <- d2expEvaluate f denv
  va <- d2expEvaluate a denv
  case vf of
    DValLam denvLam (n, _) body ->
      d2expEvaluate body ((n, va) : denvLam)
    DValFix denvFix nSelf (nArg, _) body _ ->
      d2expEvaluate body ((nArg, va) : (nSelf, vf) : denvFix)
    _ -> gErr "Application of a value that is not a lambda or a fixpoint"

d2expIf :: DEnv -> DExp -> DExp -> DExp -> M DVal
d2expIf denv c a b = do
  vc <- d2expEvaluate c denv
  case vc of
    DValBtf True -> d2expEvaluate a denv
    DValBtf False -> d2expEvaluate b denv
    _ -> gErr "If condition is not a boolean"

d2expProj :: DEnv -> Int -> DExp -> M DVal
d2expProj denv i e = do
  ve <- d2expEvaluate e denv
  case ve of
    DValTupl vs
      | i >= 0, i < length vs -> pure (vs !! i)
      | otherwise ->
          gErr $ "Tuple projection index out of range: " ++ show i
    _ -> gErr "Tuple projection of a value that is not a tuple"

d2expLet :: DEnv -> [DDecl] -> Maybe DExp -> M DVal
d2expLet denv ds body = do
  denvNew <- d2eclistEvaluate ds denv
  case body of
    Nothing -> pure dValNil
    Just b -> d2expEvaluate b denvNew

d2eclEvaluate :: DDecl -> DEnv -> M DEnv
d2eclEvaluate decl denv = case decl of
  DBind n e -> do
    v <- d2expEvaluate e denv
    pure ((n, v) : denv)
  DLocal declsImpl declsBind -> do
    denvHead <- d2eclistEvaluate declsImpl denv
    denvBody <- d2eclistEvaluate declsBind denvHead
    pure (take (length denvBody - length denvHead) denvBody ++ denv)
  t@(DTDec _) ->
    gErr $ "Template Decl " ++ show t ++ " cannot be interpreted"
  t@(DImpl _) ->
    gErr $ "Template implementation " ++ show t ++ " cannot be interpreted"

d2eclistEvaluate :: [DDecl] -> DEnv -> M DEnv
d2eclistEvaluate [] denv = pure denv
d2eclistEvaluate (decl:decls) denv = do
  denvNew <- d2eclEvaluate decl denv
  d2eclistEvaluate decls denvNew
