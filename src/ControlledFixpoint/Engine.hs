{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ControlledFixpoint.Engine where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.RWS (MonadState (..))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Control.Monad.State (StateT (runStateT), evalState, gets, modify)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Writer (MonadWriter (tell), WriterT (runWriterT))
import qualified ControlledFixpoint.Common as Common
import ControlledFixpoint.Common.Msg as Msg (Msg (..), mk)
import qualified ControlledFixpoint.Freshening as Freshening
import ControlledFixpoint.Grammar
import qualified ControlledFixpoint.Unification as Unification
import ListT (ListT, cons, toList)
import Text.PrettyPrint (hang, (<+>))
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

tellT :: (Monad m) => Msg -> T m ()
tellT msg = lift . lift . lift . lift $ tell [msg]

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
    freshCounter :: Int,
    delayedGoals :: [Atom],
    activeGoals :: [Atom],
    steps :: [Step]
  }
  deriving (Show)

instance Pretty Env where
  pPrint env =
    hang "engine environment:" 2 . bullets $
      [ "gas =" <+> pPrint env.gas,
        "activeGoals =" <+> pPrint env.activeGoals,
        "delayedGoals =" <+> pPrint env.delayedGoals,
        hang "steps:" 2 . bullets $
          env.steps <&> pPrint
      ]

data Step = Step
  { goal :: Atom,
    rule :: Rule,
    sigma :: Subst,
    subgoals :: [Atom]
  }
  deriving (Show)

instance Pretty Step where
  pPrint step =
    (pPrint step.rule.name <> ":") <+> pPrint step.goal <+> "<==" <+> pPrint step.subgoals <+> "with" <+> pPrint step.sigma

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

run :: (Monad m) => Config -> Common.T m [Env]
run cfg = do
  tell [Msg.mk "Engine.run"]
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
            steps = []
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
        [ (Msg.mk "branch terminated in error")
            { Msg.contents =
                [ hang "err:" 2 (pPrint err),
                  hang "env:" 2 (pPrint env')
                ]
            }
        ]
      return []
    (Right _, env') -> do
      tell [Msg.mk "branch terminated successfully"]
      return [env']

loop :: (Monad m) => T m ()
loop = do
  tellT $ Msg.mk "--------------------------------"

  ctx <- ask

  list_goalAndActiveGoal <- gets $ extractions . activeGoals

  -- update gas
  do
    env <- get
    tellT $ Msg.mk ("gas = " <> pPrint env.gas)
    -- check gas
    when (env.gas <= 0) do
      throwError $ Msg.mk "Out of gas"
    -- update gas
    modify \env' -> env' {gas = env.gas - length list_goalAndActiveGoal}

  if null list_goalAndActiveGoal
    then return ()
    else do
      do
        ags <- gets activeGoals
        tellT $ (Msg.mk "activeGoals:") {Msg.contents = ags <&> pPrint}

      -- choose next goal to pursue
      (activeGoals', goal) <- choose list_goalAndActiveGoal
      modify \env' -> env' {activeGoals = activeGoals'}

      tellT $ Msg.mk ("processing goal" <+> pPrint goal)

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

      tellT $ Msg.mk ("attempting to use rule" <+> pPrint rule.name)

      -- try to unify rule's conclusion with goal
      (err_or_goal', sigma_uni) <- do
        tellT $
          (Msg.mk "attempting to unify goal with rule's conclusion")
            { Msg.contents =
                [ "goal      =" <+> pPrint goal,
                  "rule.conc =" <+> pPrint rule.conc
                ]
            }
        (err_or_goal', env_uni') <-
          lift . lift . lift . lift . lift $
            Unification.unifyAtom goal rule.conc
              & runExceptT
              & flip runStateT Unification.emptyEnv
        return (err_or_goal', env_uni'.sigma)

      -- kill branch if unification failed
      case err_or_goal' of
        Left err -> do
          tellT $
            (Msg.mk "failed to unify goal with rule's conclusion")
              { Msg.contents =
                  [ "err       =" <+> pPrint err,
                    "sigma_uni =" <+> pPrint sigma_uni
                  ]
              }
          reject
        Right goal' -> do
          tellT $
            (Msg.mk "unified goal with rule's conclusion")
              { Msg.contents =
                  [ "sigma_uni =" <+> pPrint sigma_uni,
                    "goal'     =" <+> pPrint goal'
                  ]
              }
          return ()

      -- apply 'sigma_uni' to environment's 'sigma', 'activeGoals', and 'delayedGoals'
      do
        tellT $
          (Msg.mk "applying unifying substitution to environment")
            { Msg.contents =
                [ "sigma_uni =" <+> pPrint sigma_uni
                ]
            }
        modify \env' ->
          env'
            { activeGoals = env'.activeGoals <&> substAtom sigma_uni,
              delayedGoals = env'.delayedGoals <&> substAtom sigma_uni
            }

      -- process each of the rule's hypotheses
      subgoals <-
        rule.hyps
          <&>>= ( \case
                    AtomHyp atom -> do
                      let atom' = atom & substAtom sigma_uni
                      modify \env' -> env' {activeGoals = env'.activeGoals <> [atom']}
                      return [atom']
                )
          <&> concat

      modify \env' -> env' {steps = Step {goal, rule, sigma = sigma_uni, subgoals} : env'.steps}

      loop

-- | Nondeterministically choose from a list.
choose :: (Monad m) => [a] -> T m a
choose = lift . lift . lift . foldr ListT.cons mempty

-- | Nondeterministically rejected branch.
reject :: (Monad m) => T m a
reject = lift . lift . lift $ mempty
