{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.Common where

import Control.Monad (unless)
import Control.Monad.Except (runExceptT)
import Control.Monad.Writer (WriterT (runWriterT))
import ControlledFixpoint.Common.Msg (Msg)
import qualified ControlledFixpoint.Engine as Engine
import Test.Tasty as Tasty
import Test.Tasty.HUnit (assertFailure, testCase)
import Text.PrettyPrint (hang, render)
import Text.PrettyPrint.HughesPJClass (Pretty (..), prettyShow)
import Utility (bullets, (&))

-- |
-- A `EngineResult` has some optional associated metadata about how the run
-- went.
data EngineResult
  = -- | Engine run resulted in at least one branch that threw anm error.
    EngineError (Maybe Msg)
  | -- | Engine run resulted in no branches that solved all goals.
    EngineFailure (Maybe [Engine.Env])
  | -- | Engine run resulted in at least one branch that solved all goals and all successful branches had no delayed goals.
    EngineSuccess (Maybe [Engine.Env])
  | -- | Engine run resulted in at least one branch that solved all goals and had some delayed goals.
    EngineSuccessWithDelays (Maybe [Engine.Env])
  deriving (Show, Eq)

-- |
-- Ignore metadata when comparing `EngineResult`s.
(~==) :: EngineResult -> EngineResult -> Bool
EngineError _ ~== EngineError _ = True
EngineFailure _ ~== EngineFailure _ = True
EngineSuccess _ ~== EngineSuccess _ = True
EngineSuccessWithDelays _ ~== EngineSuccessWithDelays _ = True
_ ~== _ = False

instance Pretty EngineResult where
  pPrint (EngineError mb_msg) = hang "error" 2 (mb_msg & maybe mempty pPrint)
  pPrint (EngineFailure mb_envs) = hang "failure" 2 (mb_envs & maybe mempty (bullets . (pPrint <$>)))
  pPrint (EngineSuccess mb_envs) = hang "success" 2 (mb_envs & maybe mempty (bullets . (pPrint <$>)))
  pPrint (EngineSuccessWithDelays mb_envs) = hang "success with delays" 2 (mb_envs & maybe mempty (bullets . (pPrint <$>)))

mkTest_Engine :: TestName -> Engine.Config -> EngineResult -> TestTree
mkTest_Engine name cfg result_expected = testCase (name <> " " <> render (pPrint result_expected)) do
  (err_or_envs, _msgs) <-
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
  {-
  assertEqual preface expected actual =
    unless (actual == expected) (assertFailure msg)
   where msg = (if null preface then "" else preface ++ "\n") ++
               "expected: " ++ show expected ++ "\n but got: " ++ show actual
    -}
  -- result_actual @?= result_expected
  unless (result_actual ~== result_expected) do
    assertFailure $ "expected: " ++ prettyShow result_expected ++ "\n but got: " ++ prettyShow result_actual
