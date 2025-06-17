{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.Common where

import Control.Monad.Except (runExceptT)
import Control.Monad.Writer (WriterT (runWriterT))
import qualified ControlledFixpoint.Engine as Engine
import Test.Tasty as Tasty
import Test.Tasty.HUnit (testCase, (@?=))
import Text.PrettyPrint.HughesPJClass (Pretty (..))
import Utility ((&))

data EngineResult
  = EngineError
  | EngineFailure
  | EngineSuccess
  deriving (Show, Eq)

instance Pretty EngineResult where
  pPrint EngineError = "[!]"
  pPrint EngineFailure = "[x]"
  pPrint EngineSuccess = "[•]"

mkTest_Engine :: TestName -> Engine.Config -> EngineResult -> TestTree
mkTest_Engine name cfg result_expected = testCase name do
  (err_or_envs, _msgs) <-
    Engine.run cfg
      & runExceptT
      & runWriterT
  let result_actual = case err_or_envs of
        Left _err -> EngineError
        Right envs
          | any (\env -> null env.failedGoals) envs -> EngineSuccess
          | otherwise -> EngineFailure
  result_actual @?= result_expected
