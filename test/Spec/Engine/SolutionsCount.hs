{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.SolutionsCount (tests) where

import ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
import Spec.Engine.Common
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "SolutionsCount"
    [ mkTest_Engine
        "v1"
        ( Engine.Config
            { initialGas = FiniteGas 50,
              strategy = DepthFirstStrategy,
              rules =
                [ Rule
                    { name = "P 1",
                      hyps = [],
                      conc = Atom "P" [ConExpr (Con "1" [])]
                    },
                  Rule
                    { name = "P 2",
                      hyps = [],
                      conc = Atom "P" [ConExpr (Con "2" [])]
                    }
                ],
              delayable = const False,
              goals = [Atom "P" ["x"]]
            }
        )
        (EngineSuccessWithSolutionsCount 2)
    ]
