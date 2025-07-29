{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ControlledFixpoint.Freshening where

import Control.Monad.State (MonadState (..), State, modify)
import ControlledFixpoint.Grammar
import Data.Function ((&))
import Utility

data Env c v = Env
  { sigma :: Subst c v,
    freshCounter_vars :: Int,
    freshCounter_goals :: Int
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

-- | Only freshens the goal's `freshIndex`.
freshenGoalIndex :: Goal a c v -> M c v (Goal a c v)
freshenGoalIndex goal = do
  freshGoalIndex' <- do
    env <- get
    modify \env' -> env' {freshCounter_goals = env'.freshCounter_goals + 1}
    return (Just env.freshCounter_goals)
  return goal {freshGoalIndex = freshGoalIndex'}

-- | Only freshens the goal's `freshIndex` if the `freshGoalIndex` is `Nothing`.
freshenGoalIndex_init :: Goal a c v -> M c v (Goal a c v)
freshenGoalIndex_init goal = do
  case goal.freshGoalIndex of
    Nothing -> freshenGoalIndex goal
    Just _ -> return goal

freshenGoal :: (Ord v) => Goal a c v -> M c v (Goal a c v)
freshenGoal goal = do
  goal' <- freshenGoalIndex goal
  atom' <- freshenAtom goal'.atom
  return goal' {atom = atom'}

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
      let freshCounter_vars' = env.freshCounter_vars + 1
          x' = VarExpr (Var s (Just env.freshCounter_vars))
      put
        env
          { sigma = env.sigma & setVar x x',
            freshCounter_vars = freshCounter_vars'
          }
      return x'
