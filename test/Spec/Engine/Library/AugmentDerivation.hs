{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.Library.AugmentDerivation (tests) where

import ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
import Data.String (IsString (fromString))
import qualified Spec.Engine.Library.Common as Common
import System.FilePath ((<.>), (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (testCase)
import Text.PrettyPrint (render, (<+>))
import Text.PrettyPrint.HughesPJClass (pPrint)

goldenDirpath :: FilePath
goldenDirpath = Common.goldenDirpath </> "AugmentDerivation"

tests :: TestTree
tests =
  testGroup
    "AugmentDerivation"
    [tests_ex1]

tests_ex1 :: TestTree
tests_ex1 =
  testGroup
    "ex1"
    [ goldenVsString
        "test1"
        (goldenDirpath </> "example" <.> "golden")
        -- ( return $ fromString $ show Engine.Config {}
        -- )
        (return $ fromString "")
    ]
