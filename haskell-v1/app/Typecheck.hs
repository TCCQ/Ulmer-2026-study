module Typecheck where

import Ast

import Data.List (nub, (\\))
import Control.Monad (unless, foldM)

import Text.PrettyPrint (render)
import qualified Data.Map.Lazy as M


data TR = TR
  { dCtx :: M.Map DName SExp
  , tCtx :: TCtx
    -- TODO need sctx?
  } deriving (Show)

type TT a = TM () TR a

initTR :: TR
initTR = TR M.empty emptyTCtx

runTT :: TT a -> Either Error a
runTT act = snd <$> runTM act () initTR

-- builtinS :: [(SName, SExp)]
-- buildinS =
--   [ ("Int", SCon "Int" [])
--   , ("Bool", SCon "Bool" [])
--   ]

builtinOp2 :: [(DName, SExp)]
builtinOp2 =
  [ ("+",  SFun (STuple [intType , intType]) intType)
  , ("-",  SFun (STuple [intType , intType]) intType)
  , ("*",  SFun (STuple [intType , intType]) intType)
  , ("<",  SFun (STuple [intType , intType]) boolType)
  , (">",  SFun (STuple [intType , intType]) boolType)
  , ("<=", SFun (STuple [intType , intType]) boolType)
  , (">=", SFun (STuple [intType , intType]) boolType)
  , ("==", SFun (STuple [intType , intType]) boolType)
  , ("!=", SFun (STuple [intType , intType]) boolType)
  ]

intType :: SExp
intType = SCon "Int" []

boolType :: SExp
boolType = SCon "Bool" []

stringType :: SExp
stringType = SCon "String" []

unitType :: SExp
unitType = STuple []

-- TODO check that it's structurally sound
-- And use exhaustively in typechecking value level
typecheck' :: SExp -> TT SExp
typecheck' s = pure s

insertName :: DName -> SExp -> TR -> TR
insertName n t c = c { dCtx = M.insert n t (dCtx c) }

class TypeCheck a where
  typecheck :: a -> TT (SExp, a)

instance TypeCheck DExp where
  typecheck e@(DInt _) = pure (intType, e)
  typecheck e@(DBool _) = pure (boolType, e)
  typecheck e@(DStr _) = pure (stringType, e)
  typecheck   (DOp1 _ _) = do
    error "TODO builtin operators"
  typecheck e@(DOp2 n l r) = do
    (lt, l') <- typecheck l
    (rt, r') <- typecheck r
    zt <- case lookup n builtinOp2 of
      Nothing -> gErr $ "Unknown binary operator\n" ++ (render $ pp n)
      Just (SFun (STuple xy) z)
        | [x, y] <- xy , x == lt && y == rt -> pure z
        | otherwise -> gErr $ "Type error in op\n" ++ (render $ pp e)
      _ -> gErr $ "Can't make sense of binary operator type\n" ++ (render $ pp e)
    pure (zt, DOp2 n l' r')
  typecheck e@(DVar n) = do
    d <- gets dCtx
    case M.lookup n d of
      Just s -> pure (s,e)
      Nothing -> unknownName n
  typecheck   (DLam (n, t) body) = do
    t' <- typecheck' t
    (bt', body') <- extend (insertName n t') $ typecheck body
    pure (SFun t' bt', DLam (n, t') body')
  typecheck e@(DFix n (a, at) body ot) = do
    at' <- typecheck' at
    ot' <- typecheck' ot
    let helper x = insertName a at' $ insertName n ot x
    (bt', body') <- extend helper $ typecheck body
    case ot' of
      SFun x y | x == at' && y == bt' ->
                   pure (ot', DFix n (a, at') body' ot')
      _ -> gErr $ "Declared type of\n" ++ render (pp e) ++ "\nDoesn't match computed type\n" ++
        render (pp (SFun at' bt'))
  typecheck e@(DApp f a) = do
    (ft, f') <- typecheck f
    (at, a') <- typecheck a
    case ft of
      SFun xt bt | xt == at -> pure (bt, DApp f' a')
                 | otherwise -> gErr $ "Wrong type in application\n" ++ (render $ pp e) ++ "\nExpected\n" ++
                   (render $ pp xt) ++ "\nSaw\n" ++ (render $ pp at)
      _ -> gErr $ "Application on non function type\n" ++ (render $ pp at)
  typecheck e@(DIf c l r) = do
    (ct, c') <- typecheck c
    (lt, l') <- typecheck l
    (rt, r') <- typecheck r
    unless (ct == boolType) $
      gErr $ "If on non boolean\n" ++ (render $ pp e)
    unless (lt == rt) $
      gErr $ "If branches are different types\n" ++ (render $ pp e)
    pure (lt, DIf c' l' r')
  typecheck   (DTuple es) = do
    (est, es') <- unzip <$> mapM typecheck es
    pure (STuple est, DTuple es')
  typecheck e@(DProj i b) = do
    (bt, b') <- typecheck b
    case bt of
      STuple ss | length ss < i ->
                  gErr $ "Projection larger than tuple length\n" ++ (render $ pp e)
                | otherwise -> pure (ss !! i, DProj i b')
      _ -> gErr $ "Project on non tuple type\n" ++ (render $ pp e)
  typecheck   (DLet [] mbody) = do
    let helper b = do
          (bt, b') <- typecheck b
          pure (bt, Just b')
    (bt, mbody') <- maybe (pure (unitType, Nothing)) helper mbody
    pure (bt, DLet [] mbody')
  typecheck   (DLet (b:rs) mbody) = do
    (t,b') <- typecheck b
    (rt, (DLet rs' mbody')) <- insertDecl (t,b') $ typecheck (DLet rs mbody)
    pure (rt, DLet (b':rs') mbody')
  typecheck e@(DTapp n sas) = do
    -- don't resolve the actual impl here, just use shape
    tctx <- gets tCtx
    (TDecl (_, ans, rt)) <- case M.lookup n $ declared tctx of
      Nothing -> gErr $ "Template not declared during typchecking\n" ++ (render $ pp n)
      Just decl -> pure decl
    subst <- foldM sMerge substEmpty =<< sequence (zipWith (\n a -> sMatch (SVar n) a) ans sas)
    computedBt <- sSubst rt subst
    pure (computedBt, e) -- TODO replacement

-- TODO this checks but does not do replacement
instance TypeCheck DDecl where
  typecheck (DBind n rhs) = do
    (t, rhs') <- typecheck rhs
    pure (t, DBind n rhs')
  typecheck e@(DTDec d@(TDecl (n, ans, rt))) = do
    tctx <- gets tCtx
    let freeVars = (nub $ freeS rt) \\ ans
    unless (null freeVars) $
      gErr $ "Template return type contains free variables\n" ++ (render $ pp e)
    case M.lookup n $ declared tctx of
      Nothing -> pure (rt, e)
      Just priorD -> do
        unless (priorD == d) $
          gErr $ "Template doesn't match prior decl\n" ++ (render $ pp priorD) ++ "verus new decl\n" ++ (render $ pp d)
        pure (rt, e)
  typecheck e@(DImpl imp@(TImpl (n, b, as, bt, _))) = do
    tctx <- gets tCtx
    decl@(TDecl (_, ans, rt)) <- case M.lookup n (declared tctx) of
      Just d -> pure d
      Nothing ->
        gErr $ "Template not already declared\n" ++ (render $ pp imp)
    subst <- foldM sMerge substEmpty =<< sequence (zipWith (\n a -> sMatch (SVar n) a) ans as)
    shapeRt <- sSubst rt subst
    unless (shapeRt == bt) $
      gErr $ "Impl doesn't match template decl\n" ++ (render $ pp imp) ++ "verus\n" ++ (render $ pp decl)
    (computedBt, _) <- typecheck b
    unless (computedBt == bt) $
      gErr $ "Type doesn't match annotation in\n" ++ (render $ pp e) ++
        "Computed body type\n" ++ (render $ pp computedBt)
    pure (bt, e)
  typecheck   (DLocal ds bs) = do
    ds' <- mapM typecheck ds
    let wrapper x = foldr insertDecl x ds'
    bs' <- wrapper $ mapM typecheck bs
    pure (error "No type on DLocal", DLocal (map snd ds') (map snd bs'))

instance TypeCheck DProgram where
  typecheck (DProgram [] main) = do
    (mt, main') <- typecheck main
    pure (mt, DProgram [] main')
  typecheck (DProgram (bind:rs) main) = do
    bind'@(_, b') <- typecheck bind
    (dt, DProgram rs' main') <- insertDecl bind' (typecheck (DProgram rs main))
    pure (dt, DProgram (b':rs') main')

insertDecl :: (SExp, DDecl) -> TT a -> TT a
insertDecl (t, (DBind n _)) act = extend (insertName n t) act
insertDecl (_, (DTDec dec@(TDecl (n, _, _)))) act =
  let helper1 x = x { declared = M.insert n dec (declared x) }
      helper2 x = x { tCtx = helper1 (tCtx x) }
  in extend helper2 act
insertDecl (_, (DImpl impl@(TImpl (n, _, _, _, _)))) act =
  let helper1 x = x { impls = M.insertWith (++) n [impl] (impls x) }
      helper2 x = x { tCtx = helper1 (tCtx x) }
  in extend helper2 act
insertDecl (_, (DLocal binds decls)) act = do
  binds' <- mapM typecheck binds
  let wrapper x = foldr insertDecl x binds'
  decls' <- wrapper $ mapM typecheck decls
  foldr (\d acc -> wrapper $ insertDecl d acc) act decls'

unknownName :: String -> TT a
unknownName n = TM $ \_ _ -> Left (ErrorName n)
