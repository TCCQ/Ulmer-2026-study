module Ast.Types where

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

data SCtx = SCtx (M.Map SName SExp)
  deriving (Show)

data TCtx =TCtx
  { declared :: M.Map TName TDecl
  , impls :: M.Map TName [TImpl]
  } deriving (Show)

type Subst = M.Map SName SExp

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

newtype TImpl = TImpl (TName, DExp, [SExp], SExp, Int)
  -- name, body, type binds in order, body type, tag
  deriving (Show)

instance Eq TImpl where
  (TImpl (_,_,_,_,i)) == (TImpl (_,_,_,_,j)) = i == j

newtype TDecl = TDecl (TName, [SName], SExp)
  -- name, arg var names, output type over arg names
  deriving (Show, Eq)

type TBlacklist = [Int]

data DDecl
  = DBind DName DExp
  | DTDec TDecl
  | DImpl TImpl
  | DLocal [DDecl] [DDecl]
  deriving (Show, Eq)

data DProgram
  = DProgram [DDecl] DExp
  deriving (Show, Eq)

data Error
  = ErrorGeneric String
  | ErrorName String
  | ErrorContext Error String

instance Show Error where
  show (ErrorGeneric e) = e
  show (ErrorName n) = "Unknown name " ++ n ++ " encountered"
  show (ErrorContext e s) = show e ++ '\n':s

type M a = TM () (SCtx, TCtx, TBlacklist) a

data TM s r a = TM { runTM :: s -> r -> Either Error (s,a) }

instance Functor (TM s r) where
  fmap f (TM act) = TM $ \s r -> fmap (\(s,x) -> (s,f x)) $ act s r

instance Applicative (TM s r) where
  pure x = TM $ \s _ -> Right (s, x)
  f <*> x = TM $ \s r -> do
    (s',f') <- runTM f s r
    (s'', x') <- runTM x s' r
    Right (s'', f' x')

instance Monad (TM s r) where
  return = pure
  x >>= f = TM $ \s r -> do
    (s', x') <- runTM x s r
    (s'', y) <- runTM (f x') s' r
    Right (s'', y)

instance MonadFail (TM s r) where
  fail s = gErr s

get :: TM s r r
get = TM $ \s r -> Right (s,r)

gets :: (r -> a) -> TM s r a
gets f = TM $ \s r -> Right (s, f r)

extend :: (r1 -> r2) -> TM s r2 a -> TM s r1 a
extend f act = TM $ \s r -> runTM act s (f r)

load :: TM s r s
load = TM $ \s _ -> Right (s,s)

store :: s -> TM s r ()
store s = TM $ \_ _ -> Right (s, ())

loads :: (s -> a) -> TM s r a
loads f = TM $ \s _ -> Right (s,f s)

modify :: (s -> s) -> TM s r ()
modify update = TM $ \s _ -> Right (update s, ())

gErr :: String -> TM s r a
gErr s = TM $ \_ _ -> Left (ErrorGeneric s)

catch :: TM s r a -> (a -> b) -> b -> TM s r b
catch x f d = TM $ \s r ->
  case runTM x s r of
    Left _ -> Right (s, d)
    Right (s,y) -> Right (s, f y)

extendMsg :: TM s r a -> String -> TM s r a
extendMsg x m = TM $ \s r ->
  case runTM x s r of
    Left e -> Left (ErrorContext e m)
    Right y -> Right y

emptyTCtx :: TCtx
emptyTCtx = TCtx M.empty M.empty

initEnv :: (SCtx, TCtx, TBlacklist)
initEnv = (SCtx M.empty, emptyTCtx, [])

runM :: M a -> Either Error a
runM act = snd <$> runTM act () initEnv
