{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ControlledFixpoint.Freshening where

import Control.Monad.State (MonadState (..), State)
import ControlledFixpoint.Grammar
import Utility

data Env = Env
  { sigma :: Subst,
    freshCounter :: Int
  }

type M = State Env

freshenRule :: Rule -> M Rule
freshenRule rule = do
  hyps' <- rule.hyps <&>>= freshenHyp
  conc' <- rule.conc & freshenAtom
  return
    rule
      { hyps = hyps',
        conc = conc'
      }

freshenHyp :: Hyp -> M Hyp
freshenHyp (AtomHyp r) = AtomHyp <$> (r & freshenAtom)

freshenAtom :: Atom -> M Atom
freshenAtom (Atom a es) = Atom a <$> (es <&>>= freshenExpr)

freshenExpr :: Expr -> M Expr
freshenExpr (VarExpr x) = freshenVar x
freshenExpr (ConExpr (Con c es)) = ConExpr . Con c <$> (es <&>>= freshenExpr)

freshenVar :: Var -> M Expr
freshenVar x@(Var s _) = do
  env <- get
  case x & substVar env.sigma of
    Just x' -> return x'
    Nothing -> do
      let freshCounter' = env.freshCounter + 1
          x' = VarExpr (Var s (Just freshCounter'))
      put
        env
          { sigma = env.sigma & setVar x x',
            freshCounter = freshCounter'
          }
      return x'
