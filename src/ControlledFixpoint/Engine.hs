{-# HLINT ignore "Redundant $" #-}
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
import Control.Monad.State (StateT (StateT, runStateT), execStateT, gets, modify, runState)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Writer (MonadWriter)
import qualified Control.Monad.Writer as Writer
import qualified ControlledFixpoint.Common as Common
import ControlledFixpoint.Common.Msg (Msg)
import qualified ControlledFixpoint.Common.Msg as Msg
import qualified ControlledFixpoint.Freshening as Freshening
import ControlledFixpoint.Grammar
import qualified ControlledFixpoint.Unification as Unification
import Data.Function ((&))
import Data.Functor ((<&>))
import ListT (ListT, cons, toList)
import Text.PrettyPrint (braces, comma, hang, hsep, punctuate, render, text, (<+>))
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
                ( Common.T m
                )
            )
        )
    )

liftT :: (Monad m) => Common.T m x -> T a c v m x
liftT = lift . lift . lift . lift

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
      [ hang "rules =" 2 . bullets $
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
      [ "gas =" <+> pPrint env.gas,
        "activeGoals =" <+> pPrint env.activeGoals,
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
  Writer.tell [Msg.mk 0 "Engine.run"]
  let ctx0 =
        Ctx
          { config = cfg
          }
  let env0 =
        Env
          { gas = cfg.initialGas,
            activeGoals = cfg.goals,
            suspendedGoals = mempty,
            failedGoals = mempty,
            freshCounter = 0,
            stepsRev = [],
            sigma = emptySubst
          }
  err_or_branches :: Either (Error, Env a c v) [Env a c v] <-
    loop
      & flip runReaderT ctx0
      & flip execStateT env0
      & ListT.toList
      & runExceptT

  case err_or_branches of
    Left (err, env) -> do
      return $ Left (err, env)
    Right branches -> return $ Right branches

loop :: forall a c v m. (Monad m, Ord v, Eq c, Pretty a, Pretty c, Pretty v, Eq a, Show a, Show c, Show v) => T a c v m ()
loop = do
  env <- get
  overBranches
    ( \branches -> do
        when (null branches) do
          Writer.tell
            [ (Msg.mk 1 "there were no branches at this point")
                { Msg.contents = ["env =" <+> pPrint env]
                }
            ]
    )
    loop'

loop' :: forall a c v m. (Monad m, Ord v, Eq c, Pretty a, Pretty c, Pretty v, Eq a, Show a, Show c, Show v) => T a c v m ()
loop' = do
  ctx <- ask

  -- update gas
  do
    g <- gets gas
    -- check gas
    when (g & isDepletedGas) do
      env <- get
      throwError (OutOfGas, env)
    -- update gas
    modify \env -> env {gas = env.gas & decrementGas}

  -- process next active goal
  extractNextActiveGoal >>= \case
    -- if there are no more active goals, then done and terminate this branch successfully
    Nothing -> do
      env <- get
      tell
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

      if ctx.config.shouldSuspend goal
        then do
          tell
            [ (Msg.mk 1 "suspending goal")
                { Msg.contents = ["goal =" <+> pPrint goal]
                }
            ]
          modify \env ->
            env
              { suspendedGoals = case ctx.config.strategy of
                  BreadthFirstStrategy -> env.suspendedGoals <> [goal]
                  DepthFirstStrategy -> [goal] <> env.suspendedGoals
              }
        else do
          -- freshen rule
          rule <- do
            env_freshening <- do
              env <- get
              return
                Freshening.Env
                  { sigma = emptySubst,
                    freshCounter = env.freshCounter
                  }
            rule_ <- choose ctx.config.rules
            let (rule', env_freshening') =
                  Freshening.freshenRule rule_
                    & flip runState env_freshening
            modify \env -> env {freshCounter = env_freshening'.freshCounter}
            return rule'

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
            lift . lift . lift . lift $
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
              reject
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
                    AtomHyp subgoal -> do
                      let subgoal' = subgoal & substAtom sigma_uni
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
                      substAtom sigma_uni
                        <$> env.suspendedGoals,
                    activeGoals =
                      substAtom sigma_uni
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

      loop

-- | Nondeterministically choose from a list.
choose :: (Monad m) => [x] -> T a c v m x
choose = lift . lift . foldr cons mempty

-- | Nondeterministically rejected branch.
reject :: (Monad m) => T a c v m x
reject = lift . lift $ mempty

extractNextActiveGoal :: (Monad m) => T a c v m (Maybe (Atom a c v))
extractNextActiveGoal = do
  gets activeGoals >>= \case
    [] -> return Nothing
    goal : activeGoals' -> do
      modify \env -> env {activeGoals = activeGoals'}
      return $ Just goal

overBranches :: (Monad m) => ([(x, Env a c v)] -> Common.T m ()) -> T a c v m x -> T a c v m x
overBranches f m = do
  ctx <- ask
  env <- get
  branches <-
    m
      & (`runReaderT` ctx)
      & (`runStateT` env)
      & toList
      & (lift . lift . lift)
  liftT $ f branches
  lift $ StateT $ const $ foldr cons mempty branches

tell :: (MonadWriter [Msg] m, MonadTrans t1, MonadTrans t2, MonadTrans t3) => [Msg] -> t1 (t2 (t3 m)) ()
tell = lift . lift . lift . Writer.tell
