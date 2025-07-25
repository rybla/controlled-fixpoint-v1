{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ControlledFixpoint.Freshening where

import Control.Monad.State (MonadState (..), State)
import ControlledFixpoint.Grammar
import Data.Function ((&))
import Utility

data Env c v = Env
  { sigma :: Subst c v,
    freshCounter :: Int
  }

type M c v = State (Env c v)

freshenRule :: (Ord v) => Rule a c v -> M c v (Rule a c v)
freshenRule rule = do
  hyps' <- rule.hyps <&>>= freshenHyp
  conc' <- rule.conc & freshenAtom
  return
    rule
      { hyps = hyps',
        conc = conc'
      }

freshenHyp :: (Ord v) => Hyp a c v -> M c v (Hyp a c v)
freshenHyp (GoalHyp goal) = GoalHyp <$> freshenGoal goal

freshenGoal :: (Ord v) => Goal a c v -> M c v (Goal a c v)
freshenGoal goal = do
  atom' <- freshenAtom goal.atom
  return goal {atom = atom'}

freshenAtom :: (Ord v) => Atom a c v -> M c v (Atom a c v)
freshenAtom (Atom a es) = Atom a <$> (es <&>>= freshenExpr)

freshenExpr :: (Ord v) => Expr c v -> M c v (Expr c v)
freshenExpr (VarExpr x) = freshenVar x
freshenExpr (ConExpr (Con c es)) = ConExpr . Con c <$> (es <&>>= freshenExpr)

freshenVar :: (Ord v) => Var v -> M c v (Expr c v)
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
