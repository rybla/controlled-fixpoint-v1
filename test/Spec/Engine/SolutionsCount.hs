{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
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
    [ mkTest_Engine @A @C @V
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
              exprAliases = [],
              shouldSuspend = const False,
              goals = [mkGoal 0 $ Atom "P" ["x"]]
            }
        )
        (EngineSuccessWithSolutionsCount 2)
    ]
