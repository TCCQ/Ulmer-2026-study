module Ast.Util where

import Ast.Types
import Ast.MMonad
import Ast.MError

import Control.Monad (foldM)
import Data.List (nub)

import qualified Data.Map.Lazy as M

substAtom :: SName -> SExp Name -> (Subst Name)
substAtom n r = M.singleton n r

-- TODO consider better naming or splitting subst from impl tracking?
substEmpty :: (Subst Name)
substEmpty = M.empty

sMatchMaybe :: (SExp Name) -> (SExp Name) -> TM s r (Maybe (Subst Name))
sMatchMaybe l r = catch (sMatch l r) Just Nothing

{- | Try to unify the left with the right, producing a new context
binding left vars to right exprs on success.
-}
sMatch :: (SExp Name) -> (SExp Name) -> TM s r (Subst Name)
sMatch (SVar lv) r =
  pure $ substAtom lv r
sMatch l@(SCon ln las) r@(SCon rn ras)
  | (ln == rn) =
    let folder ctx (l, r) = sMerge ctx =<< sMatch l r
    in foldM folder substEmpty (zip las ras)
  | otherwise = gErr $ "Can't match " ++ show l ++ " with " ++ show r
sMatch (SFun lf la) (SFun rf ra) = do
  m1 <- sMatch lf rf
  m2 <- sMatch la ra
  sMerge m1 m2
sMatch (STuple ls) (STuple rs) =
  let folder ctx (l, r) = sMerge ctx =<< sMatch l r
  in foldM folder substEmpty (zip ls rs)
sMatch l r = gErr $ "Can't match " ++ show l ++ " with " ++ show r

{- | Perform the substiution of variables to sexprs indicated by ctx
recursively.
-}
sSubst :: (SExp Name) -> (Subst Name) -> TM s r (SExp Name)
sSubst v@(SVar n) sctx =
  case M.lookup n sctx of
    Just s -> pure s
    Nothing -> pure v
sSubst (SCon n ss) ctx = do
  ss' <- mapM (\s -> sSubst s ctx) ss
  pure (SCon n ss')
sSubst (SFun f a) ctx = do
  f' <- sSubst f ctx
  a' <- sSubst a ctx
  pure $ SFun f' a'
sSubst (STuple ss) ctx =
  STuple <$> mapM (\s -> sSubst s ctx) ss

{- | Combine two contexts or substitutions. They must agree exactly at
every overlap. -}
sMerge :: (Subst Name) -> (Subst Name) -> TM s r (Subst Name)
sMerge ls rs = do
  let sint = M.intersectionWith (\a b -> (a,b)) ls rs
  let agree = foldr (\(a,b) acc -> acc && a == b) True sint
  () <- if agree
       then pure ()
       else gErr $ "Disagreement in static merge " ++ show ls ++ " and " ++ show rs ++ " overlap " ++ show sint
  -- same for timpls
  let dint = M.intersectionWith (\a b -> (a,b)) ls rs
  let agree = foldr (\(a,b) acc -> acc && a == b) True dint
  () <- if agree
       then pure ()
       else gErr $ "Disagreement in merge " ++ show ls ++ " and " ++ show rs ++ " overlap " ++ show dint
  pure $ M.union ls rs
  -- This is a bad implemention of this, slow and unclear. Maybe write with unionWith and explicit join after?

{- | Perform subsitution recursively by walking the dexp and applying to
   the contained sexps.
-}
sSubstD :: (DExp Name) -> (Subst Name) -> TM s r (DExp Name)
sSubstD (DLam (n,t) b) ctx = do
  t' <- sSubst t ctx
  b' <- sSubstD b ctx
  pure $ DLam (n,t') b'
sSubstD (DFix n (a,at) b t) ctx = do
  at' <- sSubst at ctx
  t' <- sSubst t ctx
  b' <- sSubstD b ctx
  pure $ DFix n (a,at') b' t'
sSubstD (DOp1 n a) ctx = DOp1 n <$> sSubstD a ctx
sSubstD (DOp2 n a b) ctx = DOp2 n <$> sSubstD a ctx <*> sSubstD b ctx
sSubstD (DApp a b) ctx = DApp <$> sSubstD a ctx <*> sSubstD b ctx
sSubstD (DIf c a b) ctx = DIf <$> sSubstD c ctx <*> sSubstD a ctx <*> sSubstD b ctx
sSubstD (DTuple ss) ctx = DTuple <$> mapM (\x -> sSubstD x ctx) ss
sSubstD (DProj i a) ctx = DProj i <$> sSubstD a ctx
sSubstD (DTapp n ts) ctx = do
  ts' <- mapM (flip sSubst ctx) ts
  pure $ DTapp n ts'
sSubstD e _ = pure e

freeS :: Eq n => (SExp n) -> [n]
freeS (SCon _ as) = nub $ as >>= freeS
freeS (SVar n) = [n]
freeS (SFun l r) = nub $ freeS l ++ freeS r
freeS (STuple as) = nub $ as >>= freeS

freshen :: Name -> TM Int r Name
freshen (s,_) = do
  i <- load
  store (i + 1)
  pure (s, i)

renameDExp :: Name -> Name -> (DExp Name) -> TM Int r (DExp Name)
renameDExp target target' = go
  where
    rn n = if n == target then target' else n
    go (DInt i) = pure $ DInt i
    go (DBool b) = pure $ DBool b
    go (DStr s) = pure $ DStr s
    go (DOp1 n a) = DOp1 (rn n) <$> go a
    go (DOp2 n a b) = DOp2 (rn n) <$> go a <*> go b
    go (DVar n) = pure $ DVar (rn n)
    go (DLam (n, at) b) = do
      b' <- go b
      pure $ DLam (rn n, at) b'
    go (DFix nf (n, at) b ot) = do
      b' <- go b
      pure $ DFix (rn nf) (rn n, at) b' ot
    go (DApp a b) = DApp <$> go a <*> go b
    go (DIf c a b) = DIf <$> go c <*> go a <*> go b
    go (DTuple es) = DTuple <$> mapM go es
    go (DProj i a) = DProj i <$> go a
    go (DLet ds mb) = DLet <$> mapM (renameDDecl target target') ds <*> traverse go mb
    go e@(DTapp _ _) = pure e

renameDDecl :: Name -> Name -> (DDecl Name) -> TM Int r (DDecl Name)
renameDDecl target target' = go
  where
    rn n = if n == target then target' else n
    go (DBind n e) = DBind (rn n) <$> renameDExp target target' e
    go (DTDec d) = pure $ DTDec d
    go (DImpl (TImpl (n, ns, b, ps, t))) =
      DImpl . (\b' -> TImpl (n, ns, b', ps, t)) <$> renameDExp target target' b
    go (DLocal ps qs) = DLocal <$> mapM go ps <*> mapM go qs

-- reserved ops -------------------------------------------------

resNamesD :: [Name]
resNamesD =
  [ ("+", 0)
  , ("-", 1)
  , ("*", 2)
  , ("<", 3)
  , (">", 4)
  , ("<=", 5)
  , (">=", 6)
  , ("==", 7)
  , ("!=", 8)
  ]

resNamesS :: [Name]
resNamesS =
  [ ("Int", 9)
  , ("Bool", 10)
  , ("String", 11)
  ]

builtinOp2 :: [(DName, (SExp Name))]
builtinOp2 =
  [ (("+", 0),  SFun (STuple [intType , intType]) intType)
  , (("-", 1),  SFun (STuple [intType , intType]) intType)
  , (("*", 2),  SFun (STuple [intType , intType]) intType)
  , (("<", 3),  SFun (STuple [intType , intType]) boolType)
  , ((">", 4),  SFun (STuple [intType , intType]) boolType)
  , (("<=", 5), SFun (STuple [intType , intType]) boolType)
  , ((">=", 6), SFun (STuple [intType , intType]) boolType)
  , (("==", 7), SFun (STuple [intType , intType]) boolType)
  , (("!=", 8), SFun (STuple [intType , intType]) boolType)
  ]

intType :: (SExp Name)
intType = SCon ("Int", 9) []

boolType :: (SExp Name)
boolType = SCon ("Bool", 10) []

stringType :: (SExp Name)
stringType = SCon ("String", 11) []

unitType :: (SExp Name)
unitType = STuple []

-- end reserved ops ---------------------------------------------
