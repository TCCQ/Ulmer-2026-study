module Ast.Types where

import qualified Data.Map.Lazy as M

import Ast.MError

type Name = (String, Int)
type SName = Name
type DName = Name
type TName = Name

data SExp n
  = SCon n [(SExp n)]
  | SVar n
  | SFun (SExp n) (SExp n)
  | STuple [(SExp n)]
  deriving (Eq, Show)

data SCtx n = SCtx (M.Map SName (SExp n))
  deriving (Show)

data TCtx n = TCtx
  { declared :: M.Map n (TDecl n)
  , impls :: M.Map n [TImpl n]
  } deriving (Show)

type Subst n = M.Map n (SExp n)

data DExp n
  = DInt Int
  | DBool Bool
  | DStr String
  | DOp1 n (DExp n)
  | DOp2 n (DExp n) (DExp n)
  | DVar n
  | DLam (n, (SExp n)) (DExp n)
  | DFix n (n, (SExp n)) (DExp n) (SExp n) -- self name, arg+type, body, overall type
  | DApp (DExp n) (DExp n)
  | DIf (DExp n) (DExp n) (DExp n)
  | DTuple [(DExp n)]
  | DProj Int (DExp n)
  | DLet [DDecl n] (Maybe (DExp n))
  | DTapp n [(SExp n)] -- type applications on template
  deriving (Show, Eq)

newtype TDecl n = TDecl (n, [n], (SExp n))
  -- name, arg var names, output type over arg names
  deriving (Show, Eq)

newtype TImpl n = TImpl (n, [n], (DExp n), [(SExp n)], Int)
  -- name, type var names that can appear in binds, body, type binds in order, tag
  deriving (Show)

instance Eq (TImpl n) where
  (TImpl (_,_,_,_,i)) == (TImpl (_,_,_,_,j)) = i == j

type TBlacklist = [Int]

data DDecl n
  = DBind n (DExp n)
  | DTDec (TDecl n)
  | DImpl (TImpl n)
  | DLocal [(DDecl n)] [(DDecl n)]
  deriving (Show, Eq)

data DProgram n
  = DProgram [(DDecl n)] (DExp n)
  deriving (Show, Eq)
