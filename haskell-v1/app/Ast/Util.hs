module Ast.Util where

import Ast.Types

import qualified Data.Map.Lazy as M
import Control.Monad (foldM)

substAtom :: SName -> SExp -> Subst
substAtom n r = M.singleton n r

-- TODO consider better naming or splitting subst from impl tracking?
substEmpty :: Subst
substEmpty = M.empty

sMatchMaybe :: SExp -> SExp -> M (Maybe Subst)
sMatchMaybe l r = catch (sMatch l r) Just Nothing

{- | Try to unify the left with the right, producing a new context
binding left vars to right exprs on success.
-}
sMatch :: SExp -> SExp -> M Subst
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
sSubst :: SExp -> Subst -> M SExp
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
sMerge :: Subst -> Subst -> M Subst
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
sSubstD :: DExp -> Subst -> M DExp
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
