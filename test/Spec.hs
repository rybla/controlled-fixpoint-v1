{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import qualified Spec.Engine.Add
import qualified Spec.Engine.ApplicativeFunctorSubtyping
import qualified Spec.Engine.Library.AugmentDerivation
import qualified Spec.Engine.DelayAndResume
import qualified Spec.Engine.Proof
import qualified Spec.Engine.SolutionsCount
import qualified Spec.Engine.Subtyping
import System.Environment (setEnv)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = do
  "TASTY_NUM_THREADS" `setEnv` "1"
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "tests"
    [ Spec.Engine.Add.tests,
      Spec.Engine.Subtyping.tests,
      Spec.Engine.DelayAndResume.tests,
      Spec.Engine.SolutionsCount.tests,
      Spec.Engine.Proof.tests,
      Spec.Engine.ApplicativeFunctorSubtyping.tests,
      Spec.Engine.Library.AugmentDerivation.tests
    ]
