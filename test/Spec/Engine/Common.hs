{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.Common where

import Control.Monad (when)
import Control.Monad.Except (runExceptT)
import Control.Monad.Writer (WriterT (runWriterT))
import qualified ControlledFixpoint.Common.Msg as Msg
import qualified ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar (Subst (unSubst))
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Spec.Config as Config
import Test.Tasty as Tasty
import Test.Tasty.HUnit (assertFailure, testCase)
import Text.PrettyPrint (Doc, brackets, hang, quotes, render, text, vcat, ($+$), (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (..), prettyShow)
import Utility (bullets, (&))

-- |
-- A `EngineResult` has some optional associated metadata about how the run
-- went.
data EngineResult
  = -- | Engine run resulted in at least one branch that threw an error.
    EngineError
  | -- | Engine run resulted in no branches that solved all goals.
    EngineFailure
  | -- |
    -- Engine run resulted in at least one branch that solved all goals and
    -- all successful branches had no delayed goals.
    EngineSuccess
  | -- |
    -- Engine run resulted in at least one branch that solved all goals and had
    -- some delayed goals.
    EngineSuccessWithDelays
  | -- | Engine run resulted in each solution branch having no delayed goals.
    EngineSuccessWithoutDelays
  | -- | Engine run resulted in at least `n` branches that solved all goals.
    EngineSuccessWithSolutionsCount Int
  | -- |
    -- Engine run resulted in each solution branch using a substitution that is
    -- a sub-substitution of the `sigma`.
    EngineSuccessWithSubst Subst
  deriving (Show, Eq)

instance Pretty EngineResult where
  pPrint EngineError = "error"
  pPrint EngineFailure = "failure"
  pPrint EngineSuccess = "success"
  pPrint EngineSuccessWithDelays = "success with delays"
  pPrint EngineSuccessWithoutDelays = "success without delays"
  pPrint (EngineSuccessWithSolutionsCount n) = "success with" <+> pPrint n <+> "solutions"
  pPrint (EngineSuccessWithSubst s) = "success with substitutions" <+> pPrint s

mkTest_Engine :: TestName -> Engine.Config -> EngineResult -> TestTree
mkTest_Engine name cfg result_expected = testCase (render (text name <+> brackets (pPrint result_expected))) do
  (err_or_envs, msgs) <-
    Engine.run cfg
      & runExceptT
      & runWriterT

  mb_err :: Maybe Doc <- case err_or_envs of
    Left err -> do
      case result_expected of
        EngineError -> return Nothing
        _ -> return $ Just $ pPrint EngineError $+$ pPrint err
    Right envs
      | envs_successful <- envs & filter \env -> null env.failedGoals,
        not (null envs_successful) ->
          case result_expected of
            EngineSuccess -> return Nothing
            EngineSuccessWithDelays ->
              let envs_successfulWithDelays = envs_successful & filter \env -> not (null env.delayedGoals)
               in if null envs_successfulWithDelays
                    then return $ Just $ pPrint EngineSuccessWithoutDelays
                    else return Nothing
            EngineSuccessWithSolutionsCount n ->
              if (envs_successful & length) == n
                then return Nothing
                else return $ Just $ pPrint (EngineSuccessWithSolutionsCount (envs_successful & length)) $+$ bullets (fmap pPrint envs)
            EngineSuccessWithSubst s ->
              let m = s & unSubst
                  m_keys = m & Map.keysSet
               in case envs_successful
                    <&> ( \env ->
                            ( env,
                              let m' = env.sigma & unSubst
                                  m'_keys = m' & Map.keysSet
                                  keys = Set.union m_keys m'_keys
                               in keys & Set.toList & foldMap \x -> case (m Map.!? x, m' Map.!? x) of
                                    (Just e, Just e') -> [(x, e, Just e') | e /= e']
                                    (Just e, Nothing) -> [(x, e, Nothing)]
                                    (Nothing, _) -> []
                            )
                        )
                      & filter (not . null) of
                    envs_mismatching ->
                      if null envs_mismatching
                        then return Nothing
                        else
                          return $
                            Just $
                              hang "success with mismatches:" 2 . bullets $
                                envs_mismatching <&> \(env, mismatches) ->
                                  hang "env and mismatches:" 2 . bullets $
                                    [ hang "env:" 2 $ pPrint env,
                                      hang "mismatches:" 2 . bullets $
                                        mismatches <&> \case
                                          (x, e, Nothing) -> quotes (pPrint x) <+> "was expected to be substituted for" <+> quotes (pPrint e) <+> "but it actually wasn't substituted"
                                          (x, e, Just e') -> quotes (pPrint x) <+> "was expected to be substituted for" <+> quotes (pPrint e) <+> "but it was actually substituted for" <+> pPrint e'
                                    ]
            _ -> return Nothing
      | otherwise -> do
          case result_expected of
            EngineFailure -> return Nothing
            _ -> return $ Just $ pPrint EngineFailure $+$ bullets (fmap pPrint envs)

  case mb_err of
    Nothing -> return ()
    Just err -> do
      case Config.verbosity of
        Config.LoggingVerbosity l -> do
          putStrLn ""
          msgs & traverse_ \msg -> when (msg.level <= l) do putStrLn $ prettyShow msg
          putStrLn ""
        _ -> return ()
      assertFailure . render $
        vcat
          [ "expected:" <+> pPrint result_expected,
            "actual:" <+> err
          ]
