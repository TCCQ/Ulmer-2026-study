module Template where

import Control.Monad (zipWithM)
import Control.Applicative ((<|>))
import Data.List (foldl')

import qualified Data.Map.Lazy as M

import Ast

chooseImpl :: TName -> [SExp] -> SCtx -> M DExp
chooseImpl n args ctx@(SCtx _ tctx) = do
  impls <- case M.lookup n tctx of
    Nothing -> gErr $ "Didn't find any implementation for " ++ show n
    Just y -> pure $ y
  let applySuitable :: DDecl -> M (Maybe DExp)
      applySuitable (DImpl n' body sBinds)
        | n == n' = do
            let helper :: SExp -> (SName, SExp) -> M (Maybe (SName, SCtx))
                helper useArg (vName, implArg) = do
                  maybeSubst <- sMatchMaybe implArg useArg -- ss from impl -> use if exists
                  pure ((\ms -> (vName,ms)) <$> maybeSubst)
            maybeSubsts <- sequence <$> zipWithM helper args sBinds
            case maybeSubsts of
              Nothing -> pure Nothing
              Just substs -> do
                -- we have a successful match, instantiate
                totalSubst <- foldr (\s acc -> acc >>= sMerge s) (pure substEmpty) (map snd substs)
                body' <- sSubstD body totalSubst
                -- TODO separate two levels of subst. one for any vars
                -- that are forall-qualified at the impl site. another
                -- for any vars that are forall-qualified by the top
                -- level template def, but are points at the impl.
                -- right now they are thrown in together, which I
                -- think is correct but not super clear

                -- TODO implement blacklisting either with separate
                -- map or by removing from the ctx before recursing
                expanded <- extendMsg (templateExpand body' ctx) $ "in resolving template " ++ show n ++ " at " ++ show args
                pure $ Just expanded
        | otherwise =
          gErr $ "Timpl context on " ++ show n ++ " points to impl with name " ++ show n'
      applySuitable x =
        gErr $ "Timpl context on " ++ show n ++ " points to non impl " ++ show x
  let thread acc impl = do
        acc' <- acc
        x <- applySuitable impl
        pure $ acc' <|> x
  mExpansion <- foldl' thread (pure Nothing) impls
  case mExpansion of
    Nothing -> gErr $ "No matching impl for " ++ show n ++ " at " ++ show args ++ " in " ++ show tctx
    Just body -> pure body


insertImpl :: TName -> DExp -> [(SName, SExp)] -> SCtx -> M SCtx
insertImpl n b args (SCtx sctx tctx) = do
  case M.lookup n sctx of
    Nothing ->
      let decl = DImpl n b args
          tctx' = M.insertWith (++) n [decl] tctx
      in pure $ SCtx sctx tctx'
    Just _ -> gErr $ "Name " ++ show n ++ " appears in both tctx and sctx."

class Expandable d where
  templateExpand :: d -> SCtx -> M d

instance Expandable DDecl where
  templateExpand (DBind n e) ctx = DBind n <$> templateExpand e ctx
  templateExpand (DImpl n b args) ctx = do
    -- we don't include this impl when expanding the body
    b' <- templateExpand b ctx
    pure $ DImpl n b' args
  templateExpand (DLocal ds bs) ctx = do
    -- not mutually recursive
    ds' <- mapM (flip templateExpand ctx) ds
    ctx' <- foldr (\(DImpl n b args) acc -> acc >>= insertImpl n b args ) (pure ctx) ds'
    bs' <- mapM (flip templateExpand ctx') bs
    pure $ DLocal ds' bs'

instance Expandable DExp where
  templateExpand (DInt i) _ = pure (DInt i)
  templateExpand (DBool b) _ = pure (DBool b)
  templateExpand (DStr s) _ = pure (DStr s)
  templateExpand (DVar v) _ = pure (DVar v)
  templateExpand (DOp1 n a) ctx = DOp1 n <$> templateExpand a ctx
  templateExpand (DOp2 n a b) ctx =
    DOp2 n <$> templateExpand a ctx <*> templateExpand b ctx
  templateExpand (DLam nt b) ctx = DLam nt <$> templateExpand b ctx
  templateExpand (DFix n nt b ot) ctx = do
    b' <- templateExpand b ctx
    pure $ DFix n nt b' ot
  templateExpand (DApp a b) ctx =
    DApp <$> templateExpand a ctx <*> templateExpand b ctx
  templateExpand (DIf c a b) ctx =
    DIf <$> templateExpand c ctx
         <*> templateExpand a ctx
         <*> templateExpand b ctx
  templateExpand (DTuple ss) ctx =
    DTuple <$> mapM (flip templateExpand ctx) ss
  templateExpand (DProj i a) ctx = DProj i <$> templateExpand a ctx
  templateExpand (DLet ds body) ctx = do
    -- not mutually recursive: the arms are expanded in the caller's
    -- context, and only afterwards contribute their impls to it
    ds' <- mapM (flip templateExpand ctx) ds
    ctx' <- foldr
      (\decl acc -> case decl of
         DImpl n b args -> acc >>= insertImpl n b args
         _ -> acc)
      (pure ctx)
      ds'
    body' <- traverse (flip templateExpand ctx') body
    pure $ DLet ds' body'
  templateExpand (DTapp n ts) ctx = do
    resolved <- chooseImpl n ts ctx
    templateExpand resolved ctx
