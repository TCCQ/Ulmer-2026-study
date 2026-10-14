module Ast.MMonad where

import Ast.MError

import Ast.Types

type M a = TM Int (SCtx Name, TCtx Name, TBlacklist) a

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
