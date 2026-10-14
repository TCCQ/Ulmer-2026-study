{-# LANGUAGE FlexibleInstances #-}
module Template
  ( expandProgram
  , stripProgram

  , insertDecl
  , templateExpand
  , chooseImpl
  )
where

import Control.Monad (zipWithM)
import Control.Applicative ((<|>))
import Data.List (foldl')

import qualified Data.Map.Lazy as M

import Ast

blacklist :: Int -> M a -> M a
blacklist tag act = extend (\(a,b,bl) -> (a,b,tag:bl)) act

applySnapshot :: TCtx Name -> M a -> M a
applySnapshot snapshot act = do
  extend (\(s,_,bl) -> (s,snapshot, bl)) act


chooseImpl :: TName -> [(SExp Name)] -> M ((DExp Name), Int)
chooseImpl n args = do
  (tctx, bl) <- gets (\(_,x,bl) -> (x,bl))
  (impls, (TDecl (_,snames,_))) <- case (M.lookup n (impls tctx), M.lookup n (declared tctx)) of
    (Nothing,_) -> gErr $ "Didn't find any implementation for " ++ show n
    (_,Nothing) -> gErr $ "Didn't find any declartion for " ++ show n
    (Just y, Just d) -> pure $ (y, d)
  let applySuitable :: (TImpl Name) -> TCtx Name -> M (Maybe ((DExp Name), Int))
      applySuitable (TImpl (n', _, body, sBinds, tag)) snapshot
        | n == n' && not (elem tag bl) = do
            let helper :: (SExp Name) -> (SName, (SExp Name)) -> M (Maybe (SName, (Subst Name)))
                helper useArg (vName, implArg) = do
                  maybeSubst <- sMatchMaybe implArg useArg -- ss from impl -> use if exists
                  pure ((\ms -> (vName,ms)) <$> maybeSubst)
            maybeSubsts <- sequence <$> zipWithM helper args (zip snames sBinds)
            case maybeSubsts of
              Nothing -> pure Nothing
              Just substs -> do
                {- we have a successful match, instantiate -}
                totalSubst <- foldr (\s acc -> acc >>= sMerge s) (pure substEmpty) (map snd substs)
                body' <- sSubstD body totalSubst
                expanded <- flip extendMsg ("in resolving template " ++ show n ++ " at " ++ show args) $
                  (blacklist tag $ applySnapshot snapshot $ templateExpand body')
                pure $ Just (expanded, tag)
        | n == n' = {- blacklisted -} pure Nothing
        | otherwise =
          gErr $ "Timpl context on " ++ show n ++ " points to impl with name " ++ show n'
  let thread acc (impl, snapshot) = do
        acc' <- acc
        x <- applySuitable impl snapshot
        pure $ acc' <|> x
  mExpansion <- foldl' thread (pure Nothing) impls
  case mExpansion of
    Nothing -> gErr $ "No matching impl for " ++ show n ++ " at " ++ show args ++ " in " ++ show tctx
    Just body -> pure body

insertDecl :: (DDecl Name) -> M a -> M a
insertDecl (DTDec d@(TDecl (name, _, _))) act = do
  let update (sc,x,bl) = (sc, x { declared = M.insert name d (declared x)}, bl)
  extend update act
insertDecl (DImpl d@(TImpl (name, _, _, _, _))) act = do
  (s@(SCtx sctx), tctx, bl) <- get
  ctx' <- case (M.lookup name sctx, M.lookup name (declared tctx)) of
    (Nothing, Just _) ->
      let impls' = M.insertWith (++) name [(d, tctx)] (impls tctx)
      in pure (s, tctx { impls = impls' }, bl)
    (Just _, _) -> gErr $ "Name " ++ show name ++ " appears in both tctx and sctx."
    (_, Nothing) -> gErr $ "Name " ++ show name ++ " implemented but not declared"
  extend (const ctx') act
insertDecl _ act = act

class Expandable d where
  templateExpand :: d -> M d

{-
Idea is to preserve the ordering of decls, but promote each dbind to
be visible anywhere any following impl might be visible
(transitively). Specifically we promote any prefix of dbinds in the
private block to the enclosing block, keep the subsequent span of
tdecls/timpls in the private, and push any dbinds in the tail down
into the public block.

We rely on the tags on names to prevent this visibility promotion from
affecting any code that should not be able to see it.
-}
expandDecl :: DDecl Name -> M [DDecl Name]
expandDecl t@(DTDec _) = pure [t]
expandDecl e@(DImpl _) = pure [e] --don't do any expansion eagerly
expandDecl   (DBind n e) = do
  x <- DBind n <$> templateExpand e
  pure [x]
expandDecl (DLocal ds bs) = do
  let helper1 [] act = do
        x <- act
        pure ([], x)
      helper1 (d:ds) act = do
        d' <- expandDecl d
        (ds', act') <- foldr insertDecl (helper1 ds act) d'
        pure (d'++ds', act')
  let helper2 ds act = foldr insertDecl act ds
  (dse, ()) <- helper1 ds $ pure ()
  let isLocal (DBind _ _) = True
      isLocal _ = False
  let (localPrefix, rest) = span isLocal dse
  let (privPrefix, maybeLocal) = span (not . isLocal) rest
  if null maybeLocal
    then do
    (bse, ()) <- helper2 privPrefix $ helper1 bs (pure ())
    pure $ localPrefix ++ [DLocal privPrefix bse]
    else do
    l' <- helper2 privPrefix $ expandDecl (DLocal maybeLocal bs)
    pure $ localPrefix ++ l'

instance Expandable (DExp Name) where
  templateExpand (DInt i) = pure (DInt i)
  templateExpand (DBool b) = pure (DBool b)
  templateExpand (DStr s) = pure (DStr s)
  templateExpand (DVar v) = pure (DVar v)
  templateExpand (DOp1 n a) = DOp1 n <$> templateExpand a
  templateExpand (DOp2 n a b) =
    DOp2 n <$> templateExpand a <*> templateExpand b
  templateExpand (DLam nt b) = DLam nt <$> templateExpand b
  templateExpand (DFix n nt b ot) = do
    b' <- templateExpand b
    pure $ DFix n nt b' ot
  templateExpand (DApp a b) =
    DApp <$> templateExpand a <*> templateExpand b
  templateExpand (DIf c a b) =
    DIf <$> templateExpand c
         <*> templateExpand a
         <*> templateExpand b
  templateExpand (DTuple ss) =
    DTuple <$> mapM templateExpand ss
  templateExpand (DProj i a) = DProj i <$> templateExpand a
  templateExpand (DLet ds body) = do
    let helper1 [] act = do
          x <- act
          pure ([], x)
        helper1 (d:ds) act = do
          d' <- expandDecl d
          (ds', act') <- foldr insertDecl (helper1 ds act) d'
          pure (d'++ds', act')
    (ds', body') <- helper1 ds $ traverse templateExpand body
    pure $ DLet ds' body'
  templateExpand (DTapp n ts) =
    fst <$> chooseImpl n ts

instance Expandable (DProgram Name) where
  templateExpand (DProgram ds body) = do
    let helper1 [] act = do
          x <- act
          pure ([], x)
        helper1 (d:ds) act = do
          d' <- expandDecl d
          (ds', act') <- foldr insertDecl (helper1 ds act) d'
          pure (d'++ds', act')
    (ds', body') <- helper1 ds $ templateExpand body
    pure $ DProgram ds' body'

expandProgram :: DProgram Name -> M (DProgram Name)
expandProgram = templateExpand

-- Stripping Template decls ------------------------------------------

strip :: MonadFail m => DExp Name -> m (DExp Name)
strip e@(DInt _) = pure e
strip e@(DBool _) = pure e
strip e@(DStr _) = pure e
strip   (DOp1 n r) = DOp1 n <$> strip r
strip   (DOp2 n l r) = DOp2 n <$> strip l <*> strip r
strip e@(DVar _) = pure e
strip   (DLam x b) = DLam x <$> strip b
strip   (DFix n x b t) = DFix n x <$> strip b <*> pure t
strip   (DApp f a) = DApp <$> strip f <*> strip a
strip   (DIf c l r) = DIf <$> strip c <*> strip l <*> strip r
strip   (DTuple as) = DTuple <$> mapM strip as
strip   (DProj i r) = DProj i <$> strip r
strip   (DLet ds mb) = DLet <$> (concat <$> mapM stripDecl ds) <*> traverse strip mb
strip e@(DTapp _ _) = fail $ "Template use " ++ (rpp e) ++ " not eliminated by expansion"

stripDecl :: MonadFail m => DDecl Name -> m [DDecl Name]
stripDecl (DTDec _) = pure []
stripDecl (DImpl _) = pure []
stripDecl (DBind n rhs) = do
  rhs' <- strip rhs
  pure [DBind n rhs']
stripDecl (DLocal ds bs) = do
  ds' <- concat <$> mapM stripDecl ds
  bs' <- concat <$> mapM stripDecl bs
  if null ds' && null bs'
    then pure []
    else pure [DLocal ds' bs']

stripProgram :: MonadFail m => DProgram Name -> m (DProgram Name)
stripProgram (DProgram ds main) = do
  ds' <- concat <$> mapM stripDecl ds
  main' <- strip main
  pure (DProgram ds' main')
