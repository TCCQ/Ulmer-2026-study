module Template where

import Control.Monad (zipWithM)
import Control.Applicative ((<|>))
import Data.List (foldl')

import qualified Data.Map.Lazy as M

import Ast

blacklist :: Int -> M a -> M a
blacklist tag act = extend (\(a,b,bl) -> (a,b,tag:bl)) act

chooseImpl :: TName -> [SExp] -> M (DExp, Int)
chooseImpl n args = do
  tctx <- gets (\(_,x,_) -> x)
  (impls, (TDecl (_,snames,_))) <- case (M.lookup n (impls tctx), M.lookup n (declared tctx)) of
    (Nothing,_) -> gErr $ "Didn't find any implementation for " ++ show n
    (_,Nothing) -> gErr $ "Didn't find any declartion for " ++ show n
    (Just y, Just d) -> pure $ (y, d)
  let applySuitable :: TImpl -> M (Maybe (DExp, Int))
      applySuitable (TImpl (n', body, sBinds, _, tag))
        | n == n' = do
            let helper :: SExp -> (SName, SExp) -> M (Maybe (SName, Subst))
                helper useArg (vName, implArg) = do
                  maybeSubst <- sMatchMaybe implArg useArg -- ss from impl -> use if exists
                  pure ((\ms -> (vName,ms)) <$> maybeSubst)
            maybeSubsts <- sequence <$> zipWithM helper args (zip snames sBinds)
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
                expanded <- flip extendMsg ("in resolving template " ++ show n ++ " at " ++ show args) $
                  (templateExpand body')
                pure $ Just (expanded, tag)
        | otherwise =
          gErr $ "Timpl context on " ++ show n ++ " points to impl with name " ++ show n'
  let thread acc impl = do
        acc' <- acc
        x <- applySuitable impl
        pure $ acc' <|> x
  mExpansion <- foldl' thread (pure Nothing) impls
  case mExpansion of
    Nothing -> gErr $ "No matching impl for " ++ show n ++ " at " ++ show args ++ " in " ++ show tctx
    Just body -> pure body

insertDecl :: DDecl -> M a -> M a
insertDecl (DTDec d@(TDecl (name, _, _))) act = do
  let update (sc,x,bl) = (sc, x { declared = M.insert name d (declared x)}, bl)
  extend update act
insertDecl (DImpl d@(TImpl (name, _, _, _, _))) act = do
  (s@(SCtx sctx), tctx, bl) <- get
  ctx' <- case (M.lookup name sctx, M.lookup name (declared tctx)) of
    (Nothing, Just _) ->
      let impls' = M.insertWith (++) name [d] (impls tctx)
      in pure (s, tctx { impls = impls' }, bl)
    (Just _, _) -> gErr $ "Name " ++ show name ++ " appears in both tctx and sctx."
    (_, Nothing) -> gErr $ "Name " ++ show name ++ " implemented but not declared"
  extend (const ctx') act
insertDecl _ act = act

class Expandable d where
  templateExpand :: d -> M d

instance Expandable DDecl where
  templateExpand t@(DTDec _) = pure t
  templateExpand (DBind n e) = DBind n <$> templateExpand e
  templateExpand (DImpl (TImpl (n, b, args, ret, tag))) = do
    -- we don't include this impl when expanding the body
    b' <- blacklist tag $ templateExpand b
    pure $ DImpl (TImpl (n, b', args, ret, tag))
  templateExpand (DLocal ds bs) = do
    -- not mutually recursive
    ds' <- mapM templateExpand ds
    bs' <- foldr insertDecl (mapM templateExpand bs) ds'
    pure $ DLocal ds' bs'

instance Expandable DExp where
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
    -- not mutually recursive: the arms are expanded in the caller's
    -- context, and only afterwards contribute their impls to it
    ds' <- mapM templateExpand ds
    let bodyAction = traverse templateExpand body
    body' <- foldr insertDecl bodyAction ds'
    pure $ DLet ds' body'
  templateExpand (DTapp n ts) = do
    (resolved, tag) <- chooseImpl n ts
    blacklist tag (templateExpand resolved)

instance Expandable DProgram where
  templateExpand (DProgram [] body) = DProgram [] <$> templateExpand body
  templateExpand (DProgram (a:as) body) = do
    a' <- templateExpand a
    (DProgram as' body') <- templateExpand (DProgram as body)
    pure $ DProgram (a':as') body'
