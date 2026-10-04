module Ast where

import Control.Monad (foldM)

import qualified Data.Map.Lazy as M


type SName = String
type DName = String
type TName = String

data SExp
  = SCon SName [SExp]
  | SVar SName
  | SFun SExp SExp
  | STuple [SExp]
  deriving (Eq, Show)

data SCtx =
  SCtx (M.Map SName SExp) (M.Map TName [DDecl]) -- DDecl for template impls
  deriving (Show)
-- Need to maintiain single shared namespace on insertion

substAtom :: SName -> SExp -> SCtx
substAtom n r = SCtx (M.singleton n r) M.empty

-- TODO consider better naming or splitting subst from impl tracking?
substEmpty :: SCtx
substEmpty = SCtx M.empty M.empty

data DExp
  = DInt Int
  | DBool Bool
  | DStr String
  | DOp1 DName DExp
  | DOp2 DName DExp DExp
  | DVar DName
  | DLam (DName, SExp) DExp
  | DFix DName (DName, SExp) DExp SExp -- self name, arg+type, body, overall type
  | DApp DExp DExp
  | DIf DExp DExp DExp
  | DTuple [DExp]
  | DProj Int DExp
  | DLet [DDecl] (Maybe DExp)
  | DTapp TName [SExp] -- type applications on template
  deriving (Show, Eq)

data DDecl
  = DBind DName DExp
  | DImpl TName DExp [(SName, SExp)] -- name, body, (type binds)
  | DLocal [DDecl] [DDecl]
  deriving (Show, Eq)

data DProgram
  = DProgram [DDecl] DExp
  deriving (Show, Eq)

data Error
  = ErrorGeneric String
  | ErrorContext Error String

instance Show Error where
  show (ErrorGeneric e) = e
  show (ErrorContext e s) = show e ++ '\n':s

type M a = Either Error a

gErr :: String -> M a
gErr s = Left (ErrorGeneric s)

catch :: M a -> (a -> b) -> b -> M b
catch (Left _) _ d = pure d
catch (Right x) f _ = pure $ f x

extendMsg :: M a -> String -> M a
extendMsg e@(Right _) _ = e
extendMsg (Left err) more = Left (ErrorContext err more)

sMatchMaybe :: SExp -> SExp -> M (Maybe SCtx)
sMatchMaybe l r = catch (sMatch l r) Just Nothing

{- | Try to unify the left with the right, producing a new context
binding left vars to right exprs on success.
-}
sMatch :: SExp -> SExp -> M SCtx
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
sSubst :: SExp -> SCtx -> M SExp
sSubst v@(SVar n) (SCtx sctx _) =
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

{- | Combine two contexts or substiutions. They must agree exactly at
every overlap. -}
sMerge :: SCtx -> SCtx -> M SCtx
sMerge l@(SCtx ls lt) r@(SCtx rs rt) = do
  let sint = M.intersectionWith (\a b -> (a,b)) ls rs
  let agree = foldr (\(a,b) acc -> acc && a == b) True sint
  () <- if agree
       then pure ()
       else gErr $ "Disagreement in static merge " ++ show l ++ " and " ++ show r ++ " overlap " ++ show sint
  -- same for timpls
  let dint = M.intersectionWith (\a b -> (a,b)) ls rs
  let agree = foldr (\(a,b) acc -> acc && a == b) True dint
  () <- if agree
       then pure ()
       else gErr $ "Disagreement in merge " ++ show l ++ " and " ++ show r ++ " overlap " ++ show dint
  pure $ SCtx (M.union ls rs) (M.union lt rt)
  -- This is a bad implemention of this, slow and unclear. Maybe write with unionWith and explicit join after?

{- | Perform subsitution recursively by walking the dexp and applying to
   the contained sexps.
-}
sSubstD :: DExp -> SCtx -> M DExp
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
