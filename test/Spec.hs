{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import qualified Spec.Engine.Add
import qualified Spec.Engine.DelayAndResume
import qualified Spec.Engine.Subtyping
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "tests"
    [ Spec.Engine.Add.tests,
      Spec.Engine.Subtyping.tests,
      Spec.Engine.DelayAndResume.tests
    ]
