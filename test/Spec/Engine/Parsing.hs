{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.Parsing (tests) where

import ControlledFixpoint.Engine (Config (initialGas))
import qualified ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
import Spec.Engine.Common
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Parsing"
    [ex1_tests]

ex1_tests :: TestTree
ex1_tests = testGroup "ex1" []
  where
    mkTest :: String -> EngineResult -> TestTree
    mkTest s =
      mkTest_Engine
        (show s)
        Engine.Config
          { initialGas = 100,
            rules =
              [ Rule
                  { name = "",
                    hyps = [],
                    conc = _
                  }
              ],
            goals = [],
            delayable = const False
          }

pattern Parse :: Expr -> Expr -> Expr -> Atom
pattern Parse i s j = Atom "atom" (ConExpr (Con "Parse" [i, s, j]))

pattern String s = ConExpr (Con (ConName s) [])
