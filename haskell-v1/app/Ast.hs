module Ast
  ( module Ast.Types
  , module Ast.Util
  , module Ast.Pretty
  , module Ast.MError
  , module Ast.MMonad

  , emptyTCtx
  , initEnv
  , evalM

  ) where

import Ast.Types
import Ast.Util
import Ast.Pretty
import Ast.MError
import Ast.MMonad

import qualified Data.Map.Lazy as M

emptyTCtx :: TCtx Name
emptyTCtx = TCtx M.empty M.empty

initEnv :: (SCtx Name, TCtx Name, TBlacklist)
initEnv = (SCtx M.empty, emptyTCtx, [])

-- runM :: M a -> Either Error a
-- runM act = snd <$> runTM act 0 initEnv

evalM :: M a -> Int -> Either Error (Int, a)
evalM act nextI = runTM act nextI initEnv
