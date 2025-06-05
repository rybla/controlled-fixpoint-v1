{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ControlledFixpoint.Engine where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (MonadState (get, put), StateT (runStateT), modify)
import Control.Monad.Writer (MonadWriter (tell), WriterT (runWriterT))
import qualified ControlledFixpoint.Common as Common
import ControlledFixpoint.Common.Msg (Msg (..))
import qualified ControlledFixpoint.Common.Msg as Msg
import ControlledFixpoint.Grammar
import ListT (ListT (..), toList)
import Text.PrettyPrint.HughesPJ ((<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Utility

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Engine configuration
data Config = Config
  { initialGas :: Int,
    rules :: [Rule]
  }
  deriving (Show)

type T m =
  (ReaderT Ctx)
    ( (ExceptT Msg)
        ( (StateT Env)
            ( ListT
                ( (WriterT [Msg])
                    m
                )
            )
        )
    )

-- | Engine context
data Ctx = Ctx
  {
  }
  deriving (Show)

instance Pretty Ctx where
  pPrint = undefined

-- | Engine environment
data Env = Env
  { gas :: Int,
    sigma :: Subst,
    rules :: [Rule],
    delayedGoals :: [Rel],
    activeGoals :: [Rel]
  }
  deriving (Show)

instance Pretty Env where
  pPrint = undefined

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

run :: (Monad m) => Config -> Common.T m [Env]
run cfg = do
  let ctx = Ctx {}
  let env =
        Env
          { gas = cfg.initialGas,
            sigma = emptySubst,
            delayedGoals = mempty,
            activeGoals = mempty,
            rules = cfg.rules
          }
  (branches, logs) <-
    loop
      & flip runReaderT ctx
      & runExceptT
      & flip runStateT env
      & ListT.toList
      & runWriterT
  tell logs
  branches & traverse \case
    (Left err, env') -> throwError (err & Msg.addContent ("env' =" <+> pPrint env'))
    (Right _, env') -> return env'

loop :: (Monad m) => T m ()
loop = do
  env <- get
  -- check gas
  when (env.gas <= 0) do
    throwError $ Msg {title = "Out of gas", contents = []}
  -- update gas
  put env {gas = env.gas - 1}
  -- process next active goal
  case env.activeGoals of
    [] -> return ()
    goal : activeGoals -> do
      undefined