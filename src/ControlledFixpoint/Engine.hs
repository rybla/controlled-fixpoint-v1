{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module ControlledFixpoint.Engine where

import Control.Monad (void, when)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.RWS (MonadState (..))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Control.Monad.State (StateT (runStateT), evalState, gets, modify)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Writer (MonadWriter (tell), WriterT (runWriterT))
import qualified ControlledFixpoint.Common as Common
import ControlledFixpoint.Common.Msg (Msg (..))
import qualified ControlledFixpoint.Freshening as Freshening
import ControlledFixpoint.Grammar
import qualified ControlledFixpoint.Unification as Unification
import ListT (ListT)
import qualified ListT
import Text.PrettyPrint.HughesPJ (hang, (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Utility

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Engine configuration
data Config = Config
  { initialGas :: Int,
    rules :: [Rule],
    goals :: [Atom]
  }
  deriving (Show)

type T m =
  (ReaderT Ctx)
    ( (ExceptT Msg)
        ( (StateT Env)
            ( ListT
                ( (WriterT [Msg])
                    (Common.T m)
                )
            )
        )
    )

tell' :: (Monad m) => Msg -> T m ()
tell' msg = lift . lift . lift . lift $ tell [msg]

-- | Engine context
data Ctx = Ctx
  { rules :: [Rule]
  }
  deriving (Show)

instance Pretty Ctx where
  pPrint ctx =
    hang "engine context:" 2 . bullets $
      [ hang "rules = " 2 . bullets $
          ctx.rules <&> pPrint
      ]

-- | Engine environment
data Env = Env
  { gas :: Int,
    sigma :: Subst,
    freshCounter :: Int,
    delayedGoals :: [Atom],
    activeGoals :: [Atom]
  }
  deriving (Show)

instance Pretty Env where
  pPrint env =
    hang "engine environment:" 2 . bullets $
      [ "gas = " <> pPrint env.gas,
        "activeGoals = " <> pPrint env.activeGoals,
        "delayedGoals = " <> pPrint env.delayedGoals
      ]

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

run :: (Monad m) => Config -> Common.T m [Env]
run cfg = do
  let ctx =
        Ctx
          { rules = cfg.rules
          }
  let env =
        Env
          { gas = cfg.initialGas,
            delayedGoals = mempty,
            activeGoals = cfg.goals,
            freshCounter = 0,
            sigma = emptySubst
          }
  (branches, logs) <-
    loop
      & flip runReaderT ctx
      & runExceptT
      & flip runStateT env
      & ListT.toList
      & runWriterT
  tell logs
  branches & foldMapM \case
    (Left err, env') -> do
      tell
        [ Msg
            { title = "branch terminated in error",
              contents =
                [ hang "err:" 2 (pPrint err),
                  hang "env:" 2 (pPrint env')
                ]
            }
        ]
      return []
    (Right _, env') -> return [env']

loop :: (Monad m) => T m ()
loop = do
  tell' $ Msg "--------------------------------" mempty

  ctx <- ask
  list_goalAndActiveGoal <- gets $ extractions . activeGoals

  -- update gas
  do
    env <- get
    tell' $ Msg ("gas = " <> pPrint env.gas) mempty
    -- check gas
    when (env.gas <= 0) do
      throwError $ Msg "Out of gas" mempty
    -- update gas
    modify \env' -> env' {gas = env.gas - length list_goalAndActiveGoal}

  if null list_goalAndActiveGoal
    then return ()
    else do
      do
        ags <- gets activeGoals
        tell' $ Msg "activeGoals:" (ags <&> pPrint)

      -- choose next goal to pursue
      (activeGoals', goal) <- choose list_goalAndActiveGoal
      modify \env' -> env' {activeGoals = activeGoals'}

      tell' $ Msg ("processing goal" <+> pPrint goal) mempty

      -- choose rule to use
      rule <- do
        rule <- choose ctx.rules
        env <- get
        let env_freshening =
              Freshening.Env
                { sigma = emptySubst,
                  freshCounter = env.freshCounter
                }
        return $ Freshening.freshenRule rule & flip evalState env_freshening

      tell' $ Msg ("attempting to use rule" <+> pPrint rule.name) mempty

      -- try to unify rule's conclusion with goal
      (err_or_goal', sigma_unification) <- do
        (err_or_goal', unificationEnv') <-
          lift . lift . lift . lift . lift $
            Unification.unifyAtom goal rule.conc
              & runExceptT
              & flip runStateT Unification.emptyEnv
        return (err_or_goal', unificationEnv'.sigma)

      -- kill branch if unification failed
      case err_or_goal' of
        Left err -> do
          tell' $
            Msg
              "failed to unified goal with rule's conclusion"
              [ "err =" <+> pPrint err,
                "sigma =" <+> pPrint sigma_unification
              ]
          lift . lift . lift $ mempty
        Right goal' -> do
          tell' $
            Msg
              "unified goal with rule's conclusion"
              [ "sigma =" <+> pPrint sigma_unification,
                "goal' =" <+> pPrint goal'
              ]
          return ()

      -- apply 'sigma_unification' to environment's 'sigma', 'activeGoals', and 'delayedGoals'
      do
        sigma' <- lift . lift . lift . lift . lift . composeSubst sigma_unification =<< gets sigma
        modify \env' ->
          env'
            { sigma = sigma',
              activeGoals = env'.activeGoals <&> substAtom sigma_unification,
              delayedGoals = env'.delayedGoals <&> substAtom sigma_unification
            }

      -- process each of the rule's hypotheses
      void $
        rule.hyps <&>>= \case
          AtomHyp atom -> do
            let atom' = atom & substAtom sigma_unification
            modify \env' -> env' {activeGoals = env'.activeGoals <> [atom']}

      loop

-- | Nondeterministically choose from a list.
choose :: (Monad m) => [a] -> T m a
choose = lift . lift . lift . foldr ListT.cons mempty
