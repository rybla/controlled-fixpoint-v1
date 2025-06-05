{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ControlledFixpoint.Unification where

import Control.Monad (when, zipWithM)
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.State (StateT, get, modify)
import qualified ControlledFixpoint.Common as Common
import ControlledFixpoint.Grammar
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

type T m =
  (StateT Env)
    ( (ExceptT Error)
        (Common.T m)
    )

data Env = Env
  { sigma :: Subst
  }

data Error
  = RelsError Rel Rel
  | ExprsError Expr Expr

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

unifyRel :: (Monad m) => Rel -> Rel -> T m Rel
unifyRel (Rel r1 e1) (Rel r2 e2) = do
  when (r1 /= r2) do throwError $ RelsError (Rel r1 e1) (Rel r2 e2)
  e <- unifyExpr e1 e2
  pure (Rel r1 e)

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
