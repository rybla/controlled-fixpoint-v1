{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ControlledFixpoint.Engine where

import Control.Lens ((^.))
import Control.Monad (foldM, unless, when)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.RWS (MonadState (..))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Control.Monad.State (StateT (runStateT), execStateT, gets, modify, runState)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Writer (MonadWriter (tell), WriterT (runWriterT))
import qualified ControlledFixpoint.Common as Common
import ControlledFixpoint.Common.Msg as Msg
import qualified ControlledFixpoint.Freshening as Freshening
import ControlledFixpoint.Grammar
import qualified ControlledFixpoint.Unification as Unification
import Data.Function ((&))
import Data.Functor ((<&>))
import ListT (ListT, cons, toList)
import Text.PrettyPrint (braces, comma, hang, hsep, punctuate, quotes, render, text, (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Utility

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Engine configuration
--
-- The `shouldSuspend` predicate specifies when to suspend a goal `Atom`. For
-- example, you might want to suspend a goal that would trivially unify with
-- many rule heads, such as something like `IsTrue a` (where `a` might be
-- constrained in other goals).
data Config a c v = Config
  { initialGas :: Gas,
    rules :: [Rule a c v],
    goals :: [Atom a c v],
    shouldSuspend :: Atom a c v -> Bool,
    exprAliases :: [ExprAlias c v],
    strategy :: Strategy
  }

defaultConfig :: Config a c v
defaultConfig =
  Config
    { initialGas = InfiniteGas,
      rules = [],
      goals = [],
      shouldSuspend = const False,
      exprAliases = [],
      strategy = DepthFirstStrategy
    }

instance (Show a, Show c, Show v) => Show (Config a c v) where
  show cfg =
    render $
      "Config"
        <+> braces
          ( [ "initialGas =" <+> text (show cfg.initialGas),
              "rules =" <+> text (show cfg.rules),
              "goals =" <+> text (show cfg.goals),
              "shouldSuspend =" <+> text "<function>",
              "strategy =" <+> text (show cfg.strategy)
            ]
              & punctuate comma
              & hsep
          )

type T a c v m =
  (ReaderT (Ctx a c v))
    ( (StateT (Env a c v))
        ( ListT
            ( (ExceptT (Error, Env a c v))
                ( (WriterT [Msg])
                    (Common.T m)
                )
            )
        )
    )

tellT :: (Monad m) => Msg -> T a c v m ()
tellT msg = lift . lift . lift . lift $ tell [msg]

data Error
  = OutOfGas
  deriving (Eq, Show)

-- | Engine context
data Ctx a c v = Ctx
  { config :: Config a c v
  }

instance (Pretty a, Pretty c, Pretty v) => Pretty (Ctx a c v) where
  pPrint ctx =
    hang "engine context:" 2 . bullets $
      [ hang "rules:" 2 . bullets $
          ctx.config.rules <&> pPrint
      ]

-- | Engine environment
data Env a c v = Env
  { gas :: Gas,
    freshCounter :: Int,
    activeGoals :: [Atom a c v],
    suspendedGoals :: [Atom a c v],
    failedGoals :: [Atom a c v],
    sigma :: Subst c v,
    stepsRev :: [Step a c v]
  }
  deriving (Show, Eq)

instance (Pretty a, Pretty c, Pretty v) => Pretty (Env a c v) where
  pPrint env =
    hang "engine environment:" 2 . bullets $
      [ "gas            =" <+> pPrint env.gas,
        "activeGoals    =" <+> pPrint env.activeGoals,
        "suspendedGoals =" <+> pPrint env.suspendedGoals,
        "failedGoals    =" <+> pPrint env.failedGoals,
        hang "steps:" 2 . bullets $
          env.stepsRev & reverse <&> pPrint,
        "sigma          =" <+> pPrint env.sigma
      ]

-- | The proof-search strategy.
data Strategy
  = -- |
    -- A breadth-first strategy:
    --   - subgoals are inserted at the end of the list of subgoals to solve
    --     next, so they are to be solved after any pre-existing subgoals
    BreadthFirstStrategy
  | -- |
    -- A depth-first strategy:
    --   - subgoals are inserted at the beginning of the list of subgoals to
    --     solve next, so they are to be solved before any pre-existing subgoals
    DepthFirstStrategy
  deriving (Eq, Show, Enum, Bounded)

instance Pretty Error where
  pPrint OutOfGas = "out of gas"

data Gas
  = FiniteGas Int
  | InfiniteGas
  deriving (Eq, Show)

instance Pretty Gas where
  pPrint (FiniteGas n) = pPrint n
  pPrint InfiniteGas = "∞"

isDepletedGas :: Gas -> Bool
isDepletedGas (FiniteGas n) = n <= 0
isDepletedGas InfiniteGas = False

decrementGas :: Gas -> Gas
decrementGas (FiniteGas n) = FiniteGas (n - 1)
decrementGas InfiniteGas = InfiniteGas

data Step a c v = Step
  { goal :: Atom a c v,
    rule :: Rule a c v,
    sigma :: Subst c v,
    subgoals :: [Atom a c v]
  }
  deriving (Show, Eq)

instance (Pretty a, Pretty c, Pretty v) => Pretty (Step a c v) where
  pPrint step =
    (pPrint step.rule.name <> ":") <+> pPrint step.goal <+> "<==" <+> pPrint step.subgoals <+> "with" <+> pPrint step.sigma

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

run :: (Monad m, Ord v, Eq c, Pretty a, Pretty c, Pretty v, Eq a, Show a, Show c, Show v) => Config a c v -> Common.T m (Either (Error, Env a c v) [Env a c v])
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
            suspendedGoals = mempty,
            failedGoals = mempty,
            freshCounter = 0,
            stepsRev = [],
            sigma = emptySubst
          }
  (err_or_branches, logs) <-
    loop
      & flip runReaderT ctx
      & flip execStateT env
      & ListT.toList
      & runExceptT
      & runWriterT

  tell logs

  case err_or_branches of
    Left (err, env') -> do
      return $ Left (err, env')
    Right branches -> return $ Right branches

loop :: forall a c v m. (Monad m, Ord v, Eq c, Pretty a, Pretty c, Pretty v, Eq a, Show a, Show c, Show v) => T a c v m ()
loop = do
  ctx <- ask

  -- update gas
  do
    env <- get
    -- check gas
    when (env.gas & isDepletedGas) do
      throwError (OutOfGas, env)
    -- update gas
    modify \env' -> env' {gas = env'.gas & decrementGas}

  extractNextActiveGoal >>= \case
    -- if there are no more active goals, then done and terminate this branch successfully
    Nothing -> do
      env <- get
      tellT $
        (Msg.mk "--------------------------------")
          { Msg.contents =
              [ "status =" <+> "no more active goals",
                hang "env:" 2 (pPrint env)
              ]
          }
      return ()
    Just goal -> do
      do
        env <- get
        tellT $
          (Msg.mk "--------------------------------")
            { Msg.contents =
                [ "status =" <+> "processing goal" <+> quotes (pPrint goal),
                  hang "env:" 2 (pPrint env)
                ]
            }

      if ctx.config.shouldSuspend goal
        then do
          tellT $ (Msg.mk "suspending goal") {Msg.contents = ["goal =" <+> pPrint goal]}
          modify \env ->
            env
              { suspendedGoals = case ctx.config.strategy of
                  BreadthFirstStrategy -> env.suspendedGoals <> [goal]
                  DepthFirstStrategy -> [goal] <> env.suspendedGoals
              }
        else do
          -- freshen rule
          rule <- do
            env <- get
            let env_freshening =
                  Freshening.Env
                    { sigma = emptySubst,
                      freshCounter = env.freshCounter
                    }
            rule_ <- choose ctx.config.rules
            let (rule', env_freshening') =
                  Freshening.freshenRule rule_
                    & flip runState env_freshening
            modify \env' -> env' {freshCounter = env_freshening'.freshCounter}
            return rule'

          tellT $
            (Msg.mk "attempting to unify goal with rule's conclusion")
              { Msg.contents =
                  [ "rule      =" <+> pPrint rule.name,
                    "goal      =" <+> pPrint goal,
                    "rule.conc =" <+> pPrint rule.conc
                  ],
                level = Msg.Level 2
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
                & flip
                  runReaderT
                  Unification.Ctx
                    { exprAliases = ctx.config.exprAliases
                    }
                & runExceptT
                & flip runStateT Unification.emptyEnv

          let sigma_uni = env_uni' ^. Unification.sigma

          case err_or_goal' of
            Left err -> do
              tellT $
                (Msg.mk "failed to unify goal with rule's conclusion")
                  { Msg.contents =
                      [ "rule      =" <+> pPrint rule.name,
                        "err       =" <+> pPrint err,
                        "sigma_uni =" <+> pPrint sigma_uni,
                        "goal      =" <+> pPrint goal
                      ],
                    level = Msg.Level 2
                  }
              -- TODO: this will eliminate all logging down this branch as well... is there a better way to handle that?
              reject
            Right goal' -> do
              tellT $
                (Msg.mk "successfully unified goal with rule's conclusion")
                  { Msg.contents =
                      [ "rule         =" <+> pPrint rule.name,
                        "sigma_uni    =" <+> pPrint sigma_uni,
                        "goal         =" <+> pPrint goal,
                        "goal'        =" <+> pPrint goal',
                        "goal' (show) =" <+> text (show goal')
                      ],
                    level = Msg.Level 1
                  }

              subgoals <-
                fmap concat $
                  rule.hyps <&>>= \case
                    AtomHyp subgoal -> do
                      let subgoal' = subgoal & substAtom sigma_uni
                      return [subgoal']

              unless (null subgoals) do
                tellT $
                  (Msg.mk "new subgoals")
                    { Msg.contents = subgoals <&> pPrint,
                      Msg.level = Msg.Level 1
                    }

              suspendedGoals_old <- gets suspendedGoals

              do
                env <- get
                tellT $
                  (Msg.mk "new sigma")
                    { Msg.contents =
                        [ "old sigma =" <+> pPrint env.sigma,
                          "new sigma =" <+> pPrint (env.sigma & composeSubst_unsafe sigma_uni)
                        ]
                    }

              modify \env ->
                env
                  { suspendedGoals =
                      env.suspendedGoals
                        <&> substAtom sigma_uni,
                    activeGoals =
                      ( case ctx.config.strategy of
                          BreadthFirstStrategy -> env.activeGoals <> subgoals
                          DepthFirstStrategy -> subgoals <> env.activeGoals
                      )
                        <&> substAtom sigma_uni,
                    sigma =
                      env.sigma
                        & composeSubst_unsafe sigma_uni
                  }

              -- for each suspended goal that was refined by sigma_uni, make it active again
              suspendedGoals_curr <- gets suspendedGoals
              (activeGoals_reactivated, suspendedGoals_new) <-
                zip suspendedGoals_old suspendedGoals_curr
                  & foldM
                    ( \(activeGoals_reactivated, suspendedGoals_new) (suspendedGoal_old, suspendedGoal_curr) ->
                        if suspendedGoal_old == suspendedGoal_curr
                          then
                            -- if the suspended goal was NOT refined by the
                            -- substitution, then leave it suspended
                            return (activeGoals_reactivated, suspendedGoal_old : suspendedGoals_new)
                          else
                            -- if the suspended goal was refined by the substitution,
                            -- then reactivate it
                            return (suspendedGoal_curr : activeGoals_reactivated, suspendedGoals_new)
                    )
                    ([], [])
              modify \env ->
                env
                  { activeGoals = activeGoals_reactivated <> env.activeGoals,
                    suspendedGoals = suspendedGoals_new
                  }

              -- record step
              modify \env -> env {stepsRev = Step {goal, rule, sigma = sigma_uni, subgoals} : env.stepsRev}

              tellT $ Msg.mk "successfully applied rule"

      loop

-- | Nondeterministically choose from a list.
choose :: (Monad m) => [x] -> T a c v m x
choose = lift . lift . foldr ListT.cons mempty

-- | Nondeterministically rejected branch.
reject :: (Monad m) => T a c v m x
reject = lift . lift $ mempty

extractNextActiveGoal :: (Monad m) => T a c v m (Maybe (Atom a c v))
extractNextActiveGoal = do
  env <- get
  case env.activeGoals of
    [] -> return Nothing
    goal : activeGoals' -> do
      modify \env' -> env' {activeGoals = activeGoals'}
      return $ Just goal
