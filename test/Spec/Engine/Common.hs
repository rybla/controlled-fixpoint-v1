{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.Common where

import Control.Monad (unless, when)
import Control.Monad.Except (runExceptT)
import Control.Monad.Writer (WriterT (runWriterT))
import ControlledFixpoint.Common.Msg (Msg)
import qualified ControlledFixpoint.Engine as Engine
import qualified Spec.Config as Config
import Test.Tasty as Tasty
import Test.Tasty.HUnit (assertFailure, testCase)
import Text.PrettyPrint (brackets, hang, render, text, (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (..), prettyShow)
import Utility (bullets, (&), (<&>>=))

-- |
-- A `EngineResult` has some optional associated metadata about how the run
-- went.
data EngineResult
  = -- | Engine run resulted in at least one branch that threw an error.
    EngineError (Maybe Msg)
  | -- | Engine run resulted in no branches that solved all goals.
    EngineFailure (Maybe [Engine.Env])
  | -- | Engine run resulted in at least one branch that solved all goals and all successful branches had no delayed goals.
    EngineSuccess (Maybe [Engine.Env])
  | -- | Engine run resulted in at least one branch that solved all goals and had some delayed goals.
    EngineSuccessWithDelays (Maybe [Engine.Env])
  | -- | Engine run resulted in at least `n` branches that solved all goals.
    EngineSuccessWithSolutionsCount Int (Maybe [Engine.Env])
  deriving (Show, Eq)

mkEngineError :: EngineResult
mkEngineError = EngineError Nothing

mkEngineFailure :: EngineResult
mkEngineFailure = EngineFailure Nothing

mkEngineSuccess :: EngineResult
mkEngineSuccess = EngineSuccess Nothing

mkEngineSuccessWithDelays :: EngineResult
mkEngineSuccessWithDelays = EngineSuccessWithDelays Nothing

mkEngineSuccessWithSolutionsCount :: Int -> EngineResult
mkEngineSuccessWithSolutionsCount n = EngineSuccessWithSolutionsCount n Nothing

-- |
-- Ignore metadata when comparing `EngineResult`s.
(~==) :: EngineResult -> EngineResult -> Bool
EngineError _ ~== EngineError _ = True
EngineFailure _ ~== EngineFailure _ = True
EngineSuccess _ ~== EngineSuccess _ = True
EngineSuccessWithDelays _ ~== EngineSuccessWithDelays _ = True
EngineSuccessWithSolutionsCount _ _ ~== EngineSuccessWithSolutionsCount _ _ = True
_ ~== _ = False

instance Pretty EngineResult where
  pPrint (EngineError mb_msg) = hang "error" 2 (mb_msg & maybe mempty pPrint)
  pPrint (EngineFailure mb_envs) = hang "failure" 2 (mb_envs & maybe mempty (bullets . (pPrint <$>)))
  pPrint (EngineSuccess mb_envs) = hang "success" 2 (mb_envs & maybe mempty (bullets . (pPrint <$>)))
  pPrint (EngineSuccessWithDelays mb_envs) = hang "success with delays" 2 (mb_envs & maybe mempty (bullets . (pPrint <$>)))
  pPrint (EngineSuccessWithSolutionsCount n mb_envs) = hang ("success with" <+> pPrint n <+> "solutions") 2 (mb_envs & maybe mempty (bullets . (pPrint <$>)))

mkTest_Engine :: TestName -> Engine.Config -> EngineResult -> TestTree
mkTest_Engine name cfg result_expected = testCase (render (text name <+> brackets (pPrint result_expected))) do
  (err_or_envs, msgs) <-
    Engine.run cfg
      & runExceptT
      & runWriterT
  let result_actual = case err_or_envs of
        Left err -> EngineError (Just err)
        Right envs
          | envs_successful <- envs & filter \env -> null env.failedGoals,
            not (null envs_successful) ->
              let envs_successfulWithDelays = envs_successful & filter \env -> not (null env.delayedGoals)
               in if null envs_successfulWithDelays
                    then EngineSuccess (Just envs_successful)
                    else EngineSuccessWithDelays (Just envs_successfulWithDelays)
          | otherwise -> EngineFailure (Just envs)
  unless (result_actual ~== result_expected) do
    when (Config.verbosity >= Config.LoggingVerbosity) do
      putStrLn ""
      _ <- msgs <&>>= putStrLn . prettyShow
      return ()
    assertFailure $ "expected: " ++ prettyShow result_expected ++ "\n but got: " ++ prettyShow result_actual
