{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.Common where

import Control.Monad (when)
import Control.Monad.Except (runExceptT)
import Control.Monad.Writer (WriterT (runWriterT))
import ControlledFixpoint.Common.Msg (Msg)
import qualified ControlledFixpoint.Common.Msg as Msg
import ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar (Subst, unSubst)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Spec.Common as Common
import qualified Spec.Config as Config
import System.FilePath ((</>))
import Test.Tasty as Tasty
import Test.Tasty.HUnit (assertFailure, testCase)
import Text.PrettyPrint (Doc, brackets, hang, render, text, vcat, ($+$), (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (..), prettyShow)
import Utility (bullets, ticks)

goldenDirpath :: FilePath
goldenDirpath = Common.goldenDirpath </> "Engine"

-- |
-- A `EngineResult` has some optional associated metadata about how the run
-- went.
data EngineResult
  = -- | Engine run threw a global error (in `ControlledFixpoint.Common.T`).
    EngineErrorCatastrophic (Maybe Msg)
  | -- | Engine run threw a local error.
    EngineError Engine.Error
  | -- | Engine run resulted in no branches that solved all goals.
    EngineFailure
  | -- |
    -- Engine run resulted in at least one branch that solved all goals and
    -- all successful branches had no suspended goals.
    EngineSuccess
  | -- |
    -- Engine run resulted in at least one branch that solved all goals and had
    -- some suspended goals.
    EngineSuccessWithSuspends
  | -- | Engine run resulted in each solution branch having no suspended goals.
    EngineSuccessWithoutSuspends
  | -- | Engine run resulted in at least `n` branches that solved all goals.
    EngineSuccessWithSolutionsCount Int
  | -- |
    -- Engine run resulted in each solution branch using a substitution that is
    -- a sub-substitution of the `sigma`.
    EngineSuccessWithSubst Subst
  deriving (Show, Eq)

instance Pretty EngineResult where
  pPrint (EngineErrorCatastrophic err) = "catastrophic error:" <+> pPrint err
  pPrint (EngineError err) = "error:" <+> pPrint err
  pPrint EngineFailure = "failure"
  pPrint EngineSuccess = "success"
  pPrint EngineSuccessWithSuspends = "success with suspends"
  pPrint EngineSuccessWithoutSuspends = "success without suspends"
  pPrint (EngineSuccessWithSolutionsCount n) = "success with" <+> pPrint n <+> "solutions"
  pPrint (EngineSuccessWithSubst _) = "success with subst"

mkTest_Engine :: TestName -> Engine.Config -> EngineResult -> TestTree
mkTest_Engine name cfg result_expected = testCase (render (text name <+> brackets (pPrint result_expected))) do
  (err_or_envs, msgs) <-
    Engine.run cfg
      & runExceptT
      & runWriterT

  mb_err :: Maybe Doc <- case err_or_envs of
    Left err -> case result_expected of
      EngineErrorCatastrophic Nothing -> return Nothing
      EngineErrorCatastrophic (Just err')
        | err == err' -> return Nothing
        | otherwise -> return $ Just $ pPrint $ EngineErrorCatastrophic (Just err)
      _ -> return $ Just $ pPrint $ EngineErrorCatastrophic (Just err)
    Right (Left (err, _env)) -> case result_expected of
      EngineError err'
        | err == err' -> return Nothing
        | otherwise -> return $ Just $ pPrint $ EngineError err
      _ -> return $ Just $ pPrint $ EngineError err
    Right (Right envs)
      | envs_successful <- envs & filter \env -> null env.failedGoals,
        not (null envs_successful) ->
          case result_expected of
            EngineErrorCatastrophic _ -> return $ Just $ pPrint EngineSuccess
            EngineError _ -> return $ Just $ pPrint EngineSuccess
            EngineFailure -> return $ Just $ pPrint EngineSuccess
            --
            EngineSuccess -> return Nothing
            EngineSuccessWithSuspends ->
              let envs_successfulWithSuspends = envs_successful & filter \env -> not (null env.suspendedGoals)
               in if null envs_successfulWithSuspends
                    then return $ Just $ pPrint EngineSuccessWithoutSuspends
                    else return Nothing
            EngineSuccessWithoutSuspends ->
              let envs_successfulWithSuspends = envs_successful & filter \env -> not (null env.suspendedGoals)
               in if not $ null envs_successfulWithSuspends
                    then return $ Just $ pPrint EngineSuccessWithSuspends
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
                    & filter (\(_env, mismatches) -> not $ null mismatches) of
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
                                          (x, e, Nothing) -> "expected" <+> ticks (pPrint x <+> ":=" <+> pPrint e) <+> "but actually is wasn't substituted"
                                          (x, e, Just e') -> "expected" <+> ticks (pPrint x <+> ":=" <+> pPrint e) <+> "but actually" <+> ticks (pPrint x <+> ":=" <+> pPrint e')
                                    ]
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
          putStrLn "====[ BEGIN logs ]================"
          msgs & traverse_ \msg -> when (msg.level <= l) do putStrLn $ prettyShow msg
          putStrLn "====[ END   logs ]================"
          putStrLn ""
        _ -> return ()
      assertFailure . render $
        vcat
          [ "expected :" <+> pPrint result_expected,
            "actual   :" <+> err
          ]
