{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module ControlledFixpoint.Unification where

import Control.Monad (when, zipWithM)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State (StateT, get, gets, modify)
import Control.Monad.Trans (lift)
import qualified ControlledFixpoint.Common as Common
import ControlledFixpoint.Grammar
import Data.Function ((&))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.PrettyPrint (hang, (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Utility (bullets, fixpointEqM)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

type T m =
  ( (ReaderT Ctx)
      ( (ExceptT Error)
          ( (StateT Env)
              (Common.T m)
          )
      )
  )

liftT :: (Monad m) => Common.T m a -> T m a
liftT = lift . lift . lift

newtype Ctx = Ctx
  {exprAliases :: [ExprAlias]}

data Env = Env
  { sigma :: Subst
  }

instance Pretty Env where
  pPrint Env {..} =
    hang "Unification.Env" 2 . bullets $
      [ "sigma =" <+> pPrint sigma
      ]

emptyEnv :: Env
emptyEnv =
  Env
    { sigma = emptySubst
    }

data Error
  = AtomsError Atom Atom
  | ExprsError Expr Expr
  | OccursError Var Expr

instance Pretty Error where
  pPrint (AtomsError a1 a2) = pPrint a1 <+> "!~" <+> pPrint a2
  pPrint (ExprsError e1 e2) = pPrint e1 <+> "!~" <+> pPrint e2
  pPrint (OccursError x e) = pPrint x <+> "was unified with" <+> pPrint e <+> "recursively"

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
unifyAtom a1@(Atom c1 es1) a2@(Atom c2 es2) = do
  when (c1 /= c2) do throwError $ AtomsError a1 a2
  when ((es1 & length) /= (es2 & length)) do throwError $ AtomsError a1 a2
  let n = c1
  es <- zipWithM unifyExpr es1 es2
  pure $ Atom n es

unifyExpr :: (Monad m) => Expr -> Expr -> T m Expr
unifyExpr (VarExpr x1) e2 = do
  setVarM x1 e2
  return e2
unifyExpr e1 (VarExpr x2) = do
  setVarM x2 e1
  return e1
unifyExpr e1@(ConExpr (Con c1 es1)) e2@(ConExpr (Con c2 es2)) = do
  ctx <- ask
  if c1 /= c2
    then do
      -- if expressions are not directly directly comparable, then try applying
      -- the `ExprAlias`s to them (try `e1` first, then `e2`)
      case applyExprAliass ctx.exprAliases e1 of
        Nothing -> case applyExprAliass ctx.exprAliases e2 of
          Nothing -> throwError $ ExprsError e1 e2
          Just e2' -> unifyExpr e1 e2'
        Just e1' -> unifyExpr e2 e1'
    else do
      when ((es1 & length) /= (es2 & length)) do throwError $ ExprsError e1 e2
      let c = c1
      es <- zipWithM unifyExpr es1 es2
      pure $ con c es

-- unifyExpr e1@(ConExpr (Con c1 es1)) e2@(ConExpr (Con c2 es2)) | Just e1' <-  = _
-- unifyExpr e1 e2 = ExprsError e1 e2

normExpr :: (Monad m) => Expr -> T m Expr
normExpr = liftA2 substExpr (gets sigma) . return

normEnv :: (Monad m) => T m ()
normEnv = do
  env <- get
  sigma' <-
    env.sigma
      & fixpointEqM
        ( \s ->
            s
              & unSubst
              & Map.traverseWithKey
                ( \x e ->
                    if x `Set.member` varsExpr e
                      then throwError $ OccursError x e
                      else return $ substExpr s e
                )
              & fmap Subst
        )
  modify \env' -> env' {sigma = sigma'}
