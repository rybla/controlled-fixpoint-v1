{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Evaluate" #-}

module ControlledFixpoint.Engine where

import Control.Category ((>>>))
import Control.Lens ((^.))
import Control.Monad (foldM, unless, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Control.Monad.State (MonadState (..), StateT (..), evalStateT, execStateT, gets, modify, runState, runStateT)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Writer (MonadWriter, WriterT)
import qualified Control.Monad.Writer as Writer
import qualified ControlledFixpoint.Common as Common
import ControlledFixpoint.Common.Msg (Msg)
import qualified ControlledFixpoint.Common.Msg as Msg
import qualified ControlledFixpoint.Freshening as Freshening
import ControlledFixpoint.Grammar
import qualified ControlledFixpoint.Unification as Unification
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import ListT (ListT, cons, toList)
import Text.PrettyPrint (braces, comma, hang, hsep, punctuate, render, text, (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Utility
import Prelude hiding (init)

--------------------------------------------------------------------------------
-- types
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
    goals :: [Goal a c v],
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
              "exprAliases =" <+> text "<function>",
              "strategy =" <+> text (show cfg.strategy)
            ]
              & punctuate comma
              & hsep
          )

type T a c v m =
  (ReaderT (Ctx a c v))
    ( (StateT (Env a c v))
        ( ListT
            ( (StateT Gas) -- gas is shared among branches, rather than each branch getting it's own copy like Env
                ( (ExceptT (Error, Env a c v))
                    ( (WriterT (Trace a c v))
                        ( Common.T m
                        )
                    )
                )
            )
        )
    )

liftT :: (Monad m) => Common.T m x -> T a c v m x
liftT = lift . lift . lift . lift . lift . lift

type T' a c v m =
  (ReaderT (Ctx a c v))
    ( (StateT (Env a c v))
        ( (ExceptT (Error, Env a c v))
            ( (WriterT (Trace a c v))
                ( Common.T m
                )
            )
        )
    )

runT' :: (Monad m) => T' a c v m x -> T a c v m (x, Env a c v)
runT' m = do
  ctx <- ask
  env <- get
  m
    & (`runReaderT` ctx)
    & (`runStateT` env)
    & lift . lift . lift . lift

data Error
  = OutOfGas
  deriving (Eq, Show)

data Trace a c v = Trace
  { traceGoals :: Map GoalIndex (Goal a c v),
    traceSteps :: Map GoalIndex [Step a c v]
  }
  deriving (Eq, Show)

instance Semigroup (Trace a c v) where
  t1 <> t2 =
    Trace
      { -- t1 takes precedence
        traceGoals = Map.unionWith const t1.traceGoals t2.traceGoals,
        traceSteps = t1.traceSteps <> t2.traceSteps
      }

instance Monoid (Trace a c v) where
  mempty =
    Trace
      { traceGoals = Map.empty,
        traceSteps = Map.empty
      }

initialTrace :: Trace a c v
initialTrace =
  Trace
    { traceGoals = Map.empty,
      traceSteps = Map.empty
    }

-- | Engine context
data Ctx a c v = Ctx
  { config :: Config a c v
  }

instance (Pretty a, Pretty c, Pretty v) => Pretty (Ctx a c v) where
  pPrint ctx =
    hang "engine context:" 2 . bullets $
      [ hang "rules =" 2 . bullets $
          ctx.config.rules <&> pPrint
      ]

-- | Engine environment
data Env a c v = Env
  { freshCounter_vars :: Int,
    freshCounter_goals :: Int,
    activeGoals :: [Goal a c v],
    suspendedGoals :: [Goal a c v],
    failedGoals :: [Goal a c v],
    sigma :: Subst c v,
    stepsRev :: [Step a c v]
  }
  deriving (Show, Eq)

instance (Pretty a, Pretty c, Pretty v) => Pretty (Env a c v) where
  pPrint env =
    hang "engine environment:" 2 . bullets $
      [ "activeGoals =" <+> pPrint env.activeGoals,
        "suspendedGoals =" <+> pPrint env.suspendedGoals,
        "failedGoals =" <+> pPrint env.failedGoals,
        hang "steps =" 2 . bullets $
          env.stepsRev & reverse <&> pPrint,
        "sigma =" <+> pPrint env.sigma
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

instance Pretty Strategy where
  pPrint = text . show

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
  { goal :: Goal a c v,
    rule :: Rule a c v,
    sigma :: Subst c v,
    subgoals :: [Goal a c v]
  }
  deriving (Show, Eq)

instance (Pretty a, Pretty c, Pretty v) => Pretty (Step a c v) where
  pPrint step =
    (pPrint step.rule.name <> ":") <+> pPrint step.goal <+> "<==" <+> pPrint step.subgoals <+> "with" <+> pPrint step.sigma

--------------------------------------------------------------------------------
-- functions
--------------------------------------------------------------------------------

mkCtx :: Config a c v -> Ctx a c v
mkCtx cfg =
  Ctx
    { config = cfg
    }

mkEnv :: Config a c v -> Env a c v
mkEnv cfg =
  Env
    { activeGoals = cfg.goals,
      suspendedGoals = mempty,
      failedGoals = mempty,
      freshCounter_vars = 0,
      freshCounter_goals = 0,
      stepsRev = [],
      sigma = emptySubst
    }

-- | To run the solver, you must use either `runConfig` or `runEnv`.
runConfig ::
  (Monad m, Ord v, Eq c, Pretty a, Pretty c, Pretty v, Eq a, Show a, Show c, Show v) =>
  Config a c v ->
  WriterT (Trace a c v) (Common.T m) (Either (Error, Env a c v) [Env a c v])
runConfig cfg = do
  lift $ Writer.tell [Msg.mk 0 "Engine.run"]
  let env0 = mkEnv cfg
  runEnv cfg env0

-- | To run the solver, you must use either `runConfig` or `runEnv`.
runEnv ::
  (Monad m, Ord v, Eq c, Pretty a, Pretty c, Pretty v, Eq a, Show a, Show c, Show v) =>
  Config a c v ->
  Env a c v ->
  WriterT (Trace a c v) (Common.T m) (Either (Error, Env a c v) [Env a c v])
runEnv cfg env0 = do
  err_or_branches :: Either (Error, Env a c v) [Env a c v] <-
    start
      & (`runReaderT` mkCtx cfg)
      & (`execStateT` env0)
      & ListT.toList
      & (`evalStateT` cfg.initialGas)
      & runExceptT

  case err_or_branches of
    Left (err, env) -> return $ Left (err, env)
    Right branches -> return $ Right branches

start :: (Monad m, Ord v, Eq c, Pretty a, Pretty c, Pretty v, Eq a, Show a, Show c, Show v) => T a c v m ()
start = do
  init
  loop

init :: (Monad m) => T a c v m ()
init = do
  activeGoals' <- gets activeGoals >>= traverse (runFreshening . Freshening.freshenGoalIndex_init)
  suspendedGoals' <- gets suspendedGoals >>= traverse (runFreshening . Freshening.freshenGoalIndex_init)
  failedGoals' <- gets failedGoals >>= traverse (runFreshening . Freshening.freshenGoalIndex_init)
  modify \env ->
    env
      { activeGoals = activeGoals',
        suspendedGoals = suspendedGoals',
        failedGoals = failedGoals'
      }

loop :: forall a c v m. (Monad m, Ord v, Eq c, Pretty a, Pretty c, Pretty v, Eq a, Show a, Show c, Show v) => T a c v m ()
loop = do
  ctx <- ask

  -- update gas
  do
    g <- lift . lift $ get
    -- check gas
    when (g & isDepletedGas) do
      env <- get
      throwError (OutOfGas, env)
    -- update gas
    lift . lift $ modify decrementGas

  -- process next active goal
  extractNextActiveGoal >>= \case
    -- if there are no more active goals, then done and terminate this branch successfully
    Nothing -> do
      env <- get
      tell $
        [ (Msg.mk 1 "--------------------------------")
            { Msg.contents =
                [ "status =" <+> "no more active goals",
                  "env =" <+> pPrint env
                ]
            }
        ]
    Just goal -> do
      do
        env <- get
        tell
          [ (Msg.mk 1 "--------------------------------")
              { Msg.contents =
                  [ "status =" <+> "processing goal:" <+> pPrint goal,
                    "env =" <+> pPrint env
                  ]
              }
          ]

      if ctx.config.shouldSuspend goal.atom
        then do
          tell
            [ (Msg.mk 1 "suspending goal")
                { Msg.contents = ["goal =" <+> pPrint goal]
                }
            ]
          modify \env -> env {suspendedGoals = env.suspendedGoals <> [goal]}
        else do
          tryRules goal

      loop

tryRules :: (Monad m, Ord v, Pretty v, Pretty c, Pretty a, Eq a, Eq c) => Goal a c v -> T a c v m ()
tryRules goal = do
  ctx <- ask

  -- freshen rules
  rules <- ctx.config.rules <&>>= runFreshening . Freshening.freshenRule

  branches <- do
    env <- get
    rules
      & mapM
        ( tryRule goal
            >>> (`runReaderT` ctx)
            >>> (`runStateT` env)
            >>> (lift . lift)
        )
      & fmap
        ( filterMap
            \(success, env') ->
              if success
                then Just ((), env')
                else Nothing
        )

  if null branches
    then do
      do
        env <- get
        tell
          [ (Msg.mk 1 "failed goal since no rules can apply to it")
              { Msg.contents =
                  [ "goal =" <+> pPrint goal,
                    "env =" <+> pPrint env
                  ]
              }
          ]

      modify \env -> env {failedGoals = env.failedGoals <> [goal]}

      when (isRequiredGoal goal) do
        env <- get
        tell
          [ (Msg.mk 1 "pruning branch since faild a required goal")
              { Msg.contents =
                  [ "goal =" <+> pPrint goal,
                    "env =" <+> pPrint env
                  ]
              }
          ]
        reject
    else
      fromBranches branches

-- TODO: somehow, goal fresh indices are getting really high all the sudden in
-- some places, so that must mean that failed rule applications are still
-- modifying the environment and incrementing the fresh counter, but i can't
-- find where that's happening

-- | Returns a `Bool` indicating whether or not applying the rule was successful.
tryRule :: forall m a c v. (Monad m, Ord v, Pretty v, Pretty c, Pretty a, Eq a, Eq c) => Goal a c v -> Rule a c v -> T a c v m Bool
tryRule goal rule = do
  ctx <- ask
  tell
    [ (Msg.mk 3 "attempting to unify goal with rule's conclusion")
        { Msg.contents =
            [ "rule =" <+> pPrint rule.name,
              "goal =" <+> pPrint goal,
              "conc =" <+> pPrint rule.conc
            ]
        }
    ]
  (err_or_goal', env_uni') <-
    lift . lift . lift . lift . lift . lift $
      ( do
          atom <- Unification.unifyAtom goal.atom rule.conc
          -- NOTE: it seems odd that this is required, since I _thought_ that in the implementation of unification it incrementally applies the substitution as it is computed (viz `Unification.setVarM`)
          Unification.normEnv
          return atom
      )
        & ( `runReaderT`
              Unification.Ctx
                { exprAliases = ctx.config.exprAliases
                }
          )
        & runExceptT
        & (`runStateT` Unification.emptyEnv)

  let sigma_uni :: Subst c v
      sigma_uni = env_uni' ^. Unification.sigma
  case err_or_goal' of
    Left err -> do
      tell
        [ (Msg.mk 2 "failed to unify goal with rule's conclusion")
            { Msg.contents =
                [ "rule =" <+> pPrint rule.name,
                  "err =" <+> pPrint err,
                  "sigma_uni =" <+> pPrint sigma_uni,
                  "goal =" <+> pPrint goal
                ]
            }
        ]
      return False
    Right goal' -> do
      tell
        [ (Msg.mk 1 "successfully unified goal with rule's conclusion")
            { Msg.contents =
                [ "rule =" <+> pPrint rule.name,
                  "sigma_uni =" <+> pPrint sigma_uni,
                  "goal =" <+> pPrint goal,
                  "goal' =" <+> pPrint goal'
                ]
            }
        ]

      subgoals <-
        fmap concat $
          rule.hyps <&>>= \case
            GoalHyp subgoal -> do
              let subgoal' = subgoal & substGoal sigma_uni
              return [subgoal']

      unless (null subgoals) do
        tell
          [ (Msg.mk 1 "new subgoals")
              { Msg.contents = subgoals <&> pPrint
              }
          ]

      suspendedGoals_old <- gets suspendedGoals

      do
        env <- get
        tell
          [ (Msg.mk 1 "new sigma")
              { Msg.contents =
                  [ "old sigma =" <+> pPrint env.sigma,
                    "new sigma =" <+> pPrint (env.sigma & composeSubst_unsafe sigma_uni)
                  ]
              }
          ]

      modify \env ->
        env
          { suspendedGoals =
              substGoal sigma_uni
                <$> env.suspendedGoals,
            activeGoals =
              substGoal sigma_uni
                <$> case ctx.config.strategy of
                  BreadthFirstStrategy -> env.activeGoals <> subgoals
                  DepthFirstStrategy -> subgoals <> env.activeGoals,
            sigma =
              composeSubst_unsafe sigma_uni $
                env.sigma
          }

      -- for each suspended goal that was refined by sigma_uni, make it active again
      suspendedGoals_curr <- gets suspendedGoals
      (activeGoals_resumed, suspendedGoals_new) <-
        zip suspendedGoals_old suspendedGoals_curr
          & foldM
            ( \(activeGoals_resumed, suspendedGoals_new) (suspendedGoal_old, suspendedGoal_curr) ->
                if suspendedGoal_old == suspendedGoal_curr
                  then
                    -- if the suspended goal was NOT refined by the substitution, then leave it suspended
                    return (activeGoals_resumed, suspendedGoal_old : suspendedGoals_new)
                  else do
                    -- if the suspended goal was refined by the substitution, then resume it
                    tell
                      [ (Msg.mk 2 "resume goal")
                          { Msg.contents = [pPrint suspendedGoal_curr]
                          }
                      ]
                    return (suspendedGoal_curr : activeGoals_resumed, suspendedGoals_new)
            )
            ([], [])
      modify \env ->
        env
          { activeGoals = activeGoals_resumed <> env.activeGoals,
            suspendedGoals = suspendedGoals_new
          }

      -- record step
      modify \env -> env {stepsRev = Step {goal, rule, sigma = sigma_uni, subgoals} : env.stepsRev}

      tell
        [ (Msg.mk 1 "successfully applied rule")
            { Msg.contents =
                ["rule =" <+> pPrint rule.name]
            }
        ]

      return True

runFreshening :: (Monad m) => Freshening.M c v x -> T a c v m x
runFreshening m = do
  env_freshening <- do
    env <- get
    return
      Freshening.Env
        { sigma = emptySubst,
          freshCounter_vars = env.freshCounter_vars,
          freshCounter_goals = env.freshCounter_goals
        }
  let (x, env_freshening') = m `runState` env_freshening
  modify \env ->
    env
      { freshCounter_vars = env_freshening'.freshCounter_vars,
        freshCounter_goals = env_freshening'.freshCounter_goals
      }
  return x

-- | Nondeterministically choose from a list.
choose :: (Monad m) => [x] -> T a c v m x
choose = lift . lift . foldr cons mempty

-- | Nondeterministically pursue all branches.
fromBranches :: (Monad m) => [(x, Env a c v)] -> T a c v m x
fromBranches branches = lift $ StateT $ const $ foldr cons mempty branches

-- | Nondeterministically rejected branch.
reject :: (Monad m) => T a c v m x
reject = lift . lift $ mempty

extractNextActiveGoal :: (Monad m) => T a c v m (Maybe (Goal a c v))
extractNextActiveGoal = do
  gets activeGoals >>= \case
    [] -> return Nothing
    goal : activeGoals' -> do
      modify \env -> env {activeGoals = activeGoals'}
      return $ Just goal

tell ::
  ( MonadWriter [Msg] m,
    MonadTrans t1,
    MonadTrans t2,
    MonadTrans t3,
    MonadTrans t4,
    MonadTrans t5,
    MonadTrans t6
  ) =>
  [Msg] ->
  t1 (t2 (t3 (t4 (t5 (t6 m))))) ()
tell = lift . lift . lift . lift . lift . lift . Writer.tell
