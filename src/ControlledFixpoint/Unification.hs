{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module ControlledFixpoint.Unification where

import Control.Monad (when, zipWithM)
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.State (StateT, get, modify)
import qualified ControlledFixpoint.Common as Common
import ControlledFixpoint.Grammar
import qualified Data.Set as Set
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint), (<+>))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

type T m =
  (ExceptT Error)
    ( (StateT Env)
        (Common.T m)
    )

data Env = Env
  { sigma :: Subst
  }

emptyEnv :: Env
emptyEnv =
  Env
    { sigma = emptySubst
    }

data Error
  = AtomsError Atom Atom
  | ExprsError Expr Expr

instance Pretty Error where
  pPrint (AtomsError a1 a2) = pPrint a1 <+> "!~" <+> pPrint a2
  pPrint (ExprsError e1 e2) = pPrint e1 <+> "!~" <+> pPrint e2

setVarM :: (Monad m) => Var -> Expr -> T m ()
setVarM x e = do
  -- if 'x' occurs in 'e', then is a cyclic substitution, which is inconsistent
  when (Set.member x (varsExpr e)) do throwError $ ExprsError (VarExpr x) e
  env <- get
  e' <- case substVar env.sigma x of
    Nothing -> return e
    -- if 'x' is already substituted, then must unify the old substitute 'e'
    -- with the new substitute 'e''
    Just e' -> unifyExpr e e'
  modify \env' -> env' {sigma = setVar x e' env.sigma}

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

unifyAtom :: (Monad m) => Atom -> Atom -> T m Atom
unifyAtom (Atom a1 e1) (Atom a2 e2) = do
  when (a1 /= a2) do throwError $ AtomsError (Atom a1 e1) (Atom a2 e2)
  e <- unifyExpr e1 e2
  pure (Atom a1 e)

unifyExpr :: (Monad m) => Expr -> Expr -> T m Expr
unifyExpr (VarExpr x1) e2 = do
  setVarM x1 e2
  return e2
unifyExpr e1 (VarExpr x2) = do
  setVarM x2 e1
  return e1
unifyExpr e1@(ConExpr (Con c1 es1)) e2@(ConExpr (Con c2 es2)) = do
  when (c1 /= c2) do throwError $ ExprsError e1 e2
  let c = c1
  es <- zipWithM unifyExpr es1 es2
  pure (ConExpr (Con c es))
