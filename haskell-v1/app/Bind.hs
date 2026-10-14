module Bind
  ( bindProgram )
where

import Ast

import Control.Monad (foldM)

import qualified Data.Map.Lazy as M

data B = B
  { s :: M.Map String Int
  , t :: M.Map String (Int, TDecl Name)
  , d :: M.Map String Int
  }

initB :: B
initB =
  let sx = foldr (uncurry M.insert) M.empty resNamesS
      dx = foldr (uncurry M.insert) M.empty resNamesD
  in B sx M.empty dx

type BM a = TM Int B a

fresh :: BM Int
fresh = do
  x <- load
  store (x + 1)
  pure x

unknownName :: String -> BM a
unknownName n = TM $ \_ _ -> Left (ErrorName n)

fetchS :: String -> BM Name
fetchS n = do
  sm <- gets s
  case M.lookup n sm of
    Just i -> pure (n,i)
    Nothing -> unknownName n

fetchT :: String -> BM Name
fetchT n = do
  tm <- gets t
  case M.lookup n tm of
    Just (i,_) -> pure (n,i)
    Nothing -> unknownName n

fetchDecl :: String -> BM (TDecl Name)
fetchDecl n = do
  tm <- gets t
  case M.lookup n tm of
    Just (_,d) -> pure d
    Nothing -> unknownName n


fetchD :: String -> BM Name
fetchD n = do
  dm <- gets d
  case M.lookup n dm of
    Just i -> pure (n,i)
    Nothing -> unknownName n

insertD :: String -> BM a -> BM (Name, a)
insertD n act = do
  i <- fresh
  let helper x = x { d = M.insert n i (d x) }
  r <- extend helper $ act
  pure ((n,i), r)

insertT :: String -> Int -> TDecl Name -> BM a -> BM a
insertT n i td act = do
  let helper x = x { t = M.insert n (i,td) (t x) }
  r <- extend helper $ act
  pure r

insertS :: String -> BM a -> BM (Name, a)
insertS n act = do
  i <- fresh
  let helper x = x { s = M.insert n i (s x) }
  r <- extend helper $ act
  pure ((n,i), r)

class Bind a where
  stitch :: a String -> BM (a Name)

instance Bind SExp where
  stitch (SCon n as) = SCon <$> fetchS n <*> mapM stitch as
  stitch (SVar n) = SVar <$> fetchS n
  stitch (SFun l r) = SFun <$> stitch l <*> stitch r
  stitch (STuple ss) = STuple <$> mapM stitch ss

instance Bind DExp where
  stitch (DInt i) = pure (DInt i)
  stitch (DBool b) = pure (DBool b)
  stitch (DStr s) = pure (DStr s)
  stitch (DOp1 n r) =
    DOp1 <$> fetchD n <*> stitch r
  stitch (DOp2 n l r) =
    DOp2 <$> fetchD n <*> stitch l <*> stitch r
  stitch (DVar n) = DVar <$> fetchD n
  stitch (DLam (n, at) b) = do
    (n',b') <- insertD n (stitch b)
    at' <- stitch at
    pure $ DLam (n',at') b'
  stitch (DFix f (n, at) b tt) = do
    (f',(n',b')) <- insertD f $ insertD n $ (stitch b)
    at' <- stitch at
    tt' <- stitch tt
    pure $ DFix f' (n', at') b' tt'
  stitch (DApp f a) =
    DApp <$> stitch f <*> stitch a
  stitch (DIf c l r) =
    DIf <$> stitch c <*> stitch l <*> stitch r
  stitch (DTuple as) =
    DTuple <$> mapM stitch as
  stitch (DProj i r) =
    DProj i <$> stitch r
  stitch (DLet [] Nothing) = pure (DLet [] Nothing)
  stitch (DLet [] (Just b)) = do
    b' <- stitch b
    pure (DLet [] (Just b'))
  stitch (DLet (d:ds) mb) = do
    d' <- stitch d
    (DLet ds' mb') <- insertDecl d' $ stitch (DLet ds mb)
    pure (DLet (d':ds') mb')
  stitch (DTapp n as) =
    DTapp <$> fetchT n <*> mapM stitch as

instance Bind DDecl where
  stitch (DBind n r) = do
    r' <- stitch r
    i <- fresh
    let n' = (n, i)
    pure $ (DBind n' r')
  stitch (DTDec (TDecl (n, as, rt))) = do
    i <- fresh
    let n' = (n,i)
    let helper1 [] rt = do
          rt' <- stitch rt
          pure ([], rt')
        helper1 (a:as) rt = do
          (a', (as', rt')) <- insertS a $ helper1 as rt
          pure (a':as', rt')
    (as', rt') <- helper1 as rt
    let td' = TDecl (n', as', rt')
    pure (DTDec td')
  stitch (DImpl (TImpl (n, nvs, b, as, tag))) = do
    (TDecl (n', _, _)) <- fetchDecl n
    let withVars act = foldr
          (\v acc -> do
              (v', (vs', ac')) <- insertS v acc
              pure (v':vs', ac'))
          (do x <- act; pure ([],x)) nvs
    let bindPatterns = mapM stitch as
    let bodyact = stitch b
    (nvs', (pats', b')) <- withVars $ do
      pats' <- bindPatterns
      body' <- bodyact
      pure (pats', body')
    pure (DImpl (TImpl (n', nvs', b', pats', tag)))
  stitch (DLocal privs pubs) = do
    let helper1 [] body = do
          b' <- body
          pure ([], b')
        helper1 (d:ds) body = do
          d' <- stitch d
          (ds', b') <- insertDecl d' $ helper1 ds body
          pure (d':ds', b')
    (privs', (pubs', ())) <- helper1 privs (helper1 pubs (pure ()))

    -- let helper1 (ds', acc) d = do
    --       d' <- stitch d
    --       x' <- insertDecl d' acc
    --       pure (d':ds', x')
    --     init1 = ([], mapM stitch pubs)
    -- (privs', pubs') <- foldr helper1 init1 privs
    pure (DLocal privs' pubs')

insertDecl :: DDecl Name -> BM a -> BM a
insertDecl (DBind (n,i) _) act = do
  let helper1 m = M.insert n i m
      helper2 x = x { d = helper1 (d x) }
  extend helper2 act
insertDecl (DTDec d@(TDecl ((n,i), _, _))) act = do
  let helper1 m = M.insert n (i,d) m
      helper2 x = x { t = helper1 (t x) }
  extend helper2 act
insertDecl (DImpl _) act = act
insertDecl (DLocal _ pubs) act =
  foldr insertDecl act pubs

bindProgram :: DProgram String -> Either Error (Int, DProgram Name)
bindProgram prog =
  let action (DProgram [] main) = DProgram [] <$> stitch main
      action (DProgram (d:ds) main) = do
        d' <- stitch d
        (DProgram ds' main') <- insertDecl d' $ action (DProgram ds main)
        pure (DProgram (d':ds') main')
      startI = length resNamesS + length resNamesD + 1
  in runTM (action prog) startI initB
