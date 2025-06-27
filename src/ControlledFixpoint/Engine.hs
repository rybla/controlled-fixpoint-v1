{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module ControlledFixpoint.Engine where

import Control.Monad (foldM, unless, when)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.RWS (MonadState (..))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Control.Monad.State (StateT (runStateT), gets, modify, runState)
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
--
-- The `delayable` predicate specifies when to delay a goal `Atom`. For example,
-- you might want to delay a goal that would trivially unify with many rule
-- heads, such as something like `IsTrue a` (where `a` might be constrained in
-- other goals).
data Config = Config
  { initialGas :: Int,
    rules :: [Rule],
    goals :: [Atom],
    delayable :: Atom -> Bool
  }

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
  { config :: Config
  }

instance Pretty Ctx where
  pPrint ctx =
    hang "engine context:" 2 . bullets $
      [ hang "rules = " 2 . bullets $
          ctx.config.rules <&> pPrint
      ]

-- | Engine environment
data Env = Env
  { gas :: Int,
    freshCounter :: Int,
    activeGoals :: [Atom],
    delayedGoals :: [Atom],
    failedGoals :: [Atom],
    stepsRev :: [Step],
    sigma :: Subst
  }
  deriving (Show, Eq)

instance Pretty Env where
  pPrint env =
    hang "engine environment:" 2 . bullets $
      [ "gas =" <+> pPrint env.gas,
        "activeGoals =" <+> pPrint env.activeGoals,
        "delayedGoals =" <+> pPrint env.delayedGoals,
        "failedGoals =" <+> pPrint env.failedGoals,
        hang "steps:" 2 . bullets $
          env.stepsRev & reverse <&> pPrint,
        "sigma =" <+> pPrint env.sigma
      ]

data Step = Step
  { goal :: Atom,
    rule :: Rule,
    sigma :: Subst,
    subgoals :: [Atom]
  }
  deriving (Show, Eq)

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
          { config = cfg
          }
  let env =
        Env
          { gas = cfg.initialGas,
            activeGoals = cfg.goals,
            delayedGoals = mempty,
            failedGoals = mempty,
            freshCounter = 0,
            stepsRev = [],
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

loop :: forall m. (Monad m) => T m ()
loop = do
  tellT $ Msg.mk "--------------------------------"

  ctx <- ask

  -- update gas
  do
    env <- get
    tellT $ Msg.mk ("gas = " <> pPrint env.gas)
    -- check gas
    when (env.gas <= 0) do
      throwError $ Msg.mk "Out of gas"
    -- update gas
    modify \env' -> env' {gas = env.gas - length env.activeGoals}

  do
    ags <- gets activeGoals
    tellT $ (Msg.mk "activeGoals:") {Msg.contents = ags <&> pPrint}

  extractNextActiveGoal >>= \case
    -- if there are no more active goals, then done and terminate this branch successfully
    Nothing -> return ()
    Just goal -> do
      tellT $ Msg.mk ("processing goal" <+> pPrint goal)

      if ctx.config.delayable goal
        then do
          tellT $ (Msg.mk "delaying goal") {Msg.contents = ["goal =" <+> pPrint goal]}
          modify \env' -> env' {delayedGoals = goal : env'.delayedGoals}
        else do
          let tryRule :: Rule -> T m [(Rule, Subst, [Atom])]
              tryRule rule_ = do
                -- freshen rule
                rule <- do
                  env <- get
                  let env_freshening =
                        Freshening.Env
                          { sigma = emptySubst,
                            freshCounter = env.freshCounter
                          }
                  let (rule', env_freshening') =
                        Freshening.freshenRule rule_
                          & flip runState env_freshening
                  modify \env' -> env' {freshCounter = env_freshening'.freshCounter}
                  return rule'

                tellT $ Msg.mk ("attempting to use rule" <+> pPrint rule.name)

                tellT $
                  (Msg.mk "attempting to unify goal with rule's conclusion")
                    { Msg.contents =
                        [ "goal      =" <+> pPrint goal,
                          "rule.conc =" <+> pPrint rule.conc
                        ]
                    }
                (err_or_goal', env_uni') <-
                  lift . lift . lift . lift . lift $
                    ( do
                        atom <- Unification.unifyAtom goal rule.conc
                        -- NOTE: it seems odd that this is required, since I
                        -- _thought_ that in the implementation of unification
                        -- it incrementally applies the substitution as it is
                        -- computed (viz `Unification.setVarM`)
                        Unification.normEnv
                        return atom
                    )
                      & runExceptT
                      & flip runStateT Unification.emptyEnv

                case err_or_goal' of
                  Left err -> do
                    tellT $
                      (Msg.mk "failed to unify goal with rule's conclusion")
                        { Msg.contents =
                            [ "err       =" <+> pPrint err,
                              "sigma_uni =" <+> pPrint env_uni'.sigma
                            ]
                        }
                    return []
                  Right goal' -> do
                    tellT $
                      (Msg.mk "successfully unified goal with rule's conclusion")
                        { Msg.contents =
                            [ "sigma_uni =" <+> pPrint env_uni'.sigma,
                              "goal'     =" <+> pPrint goal'
                            ]
                        }

                    -- process the rule's hypotheses
                    let processHyps subgoals [] = return [(rule, env_uni'.sigma, subgoals)]
                        processHyps subgoals (AtomHyp subgoal : hs) = processHyps (subgoal : subgoals) hs
                    processHyps [] rule.hyps

          -- try all rules
          results <- ctx.config.rules <&>>= tryRule <&> concat

          if null results
            then do
              -- if no results for unifying the goal with rule conclusions, then
              -- this goal is unsolvable
              modify \env' -> env' {failedGoals = goal : env'.failedGoals}
            else do
              -- if there are some results for unifying the goal with rule
              -- conclusions, then for each success branch for using that rule apply
              -- 'sigma_uni' to environment's 'sigma', 'activeGoals', and
              -- 'delayedGoals'

              (rule, sigma_uni, subgoals) <- choose results

              unless (null subgoals) do
                tellT $ (Msg.mk "new subgoals") {Msg.contents = subgoals <&> pPrint}
              modify \env' -> env' {activeGoals = env'.activeGoals <> subgoals}

              tellT $
                (Msg.mk "applying unifying substitution to environment")
                  { Msg.contents =
                      [ "sigma_uni =" <+> pPrint sigma_uni
                      ]
                  }

              delayedGoals_old <- gets delayedGoals

              modify \env' ->
                env'
                  { delayedGoals = env'.delayedGoals <&> substAtom sigma_uni,
                    activeGoals = env'.activeGoals <&> substAtom sigma_uni,
                    sigma = env'.sigma & composeSubst_unsafe sigma_uni
                  }

              -- for each delayed goal that was refined by sigma_uni, make it active again

              delayedGoals_curr <- gets delayedGoals
              (activeGoals_reactivated, delayedGoals_new) <-
                zip delayedGoals_old delayedGoals_curr
                  & foldM
                    ( \(activeGoals_reactivated, delayedGoals_new) (delayedGoal_old, delayedGoal_curr) ->
                        if delayedGoal_old == delayedGoal_curr
                          then
                            -- if the delayed goal was NOT refined by the
                            -- substitution, then leave it delayed
                            return (activeGoals_reactivated, delayedGoal_old : delayedGoals_new)
                          else
                            -- if the delayed goal was refined by the substitution,
                            -- then reactivate it
                            return (delayedGoal_curr : activeGoals_reactivated, delayedGoals_new)
                    )
                    ([], [])
              modify \env' ->
                env'
                  { activeGoals = activeGoals_reactivated <> env'.activeGoals,
                    delayedGoals = delayedGoals_new
                  }

              -- record step
              modify \env' -> env' {stepsRev = Step {goal, rule, sigma = sigma_uni, subgoals} : env'.stepsRev}

      loop

-- | Nondeterministically choose from a list.
choose :: (Monad m) => [a] -> T m a
choose = lift . lift . lift . foldr ListT.cons mempty

-- | Nondeterministically rejected branch.
reject :: (Monad m) => T m a
reject = lift . lift . lift $ mempty

extractNextActiveGoal :: (Monad m) => T m (Maybe Atom)
extractNextActiveGoal = do
  env <- get
  case env.activeGoals of
    [] -> return Nothing
    goal : activeGoals' -> do
      modify \env' -> env' {activeGoals = activeGoals'}
      return $ Just goal
