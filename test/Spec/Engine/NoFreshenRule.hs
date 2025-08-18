{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Engine.NoFreshenRule (tests) where

import ControlledFixpoint.Engine
import ControlledFixpoint.Grammar
import Data.Function ((&))
import Spec.Engine.Common
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "NoFreshenRule"
    [ mkTest_Engine
        "yesFreshen"
        ( (defaultConfig @String @Int @String)
            { initialGas = FiniteGas 50,
              rules =
                [ mkRule
                    "R1"
                    [ Atom "Q" [0 :% []] & GoalHyp . mkHypGoal,
                      Atom "Q" ["x"] & GoalHyp . mkHypGoal
                    ]
                    (Atom "P" ["x"]),
                  mkRule
                    "R2"
                    []
                    (Atom "Q" ["x"])
                ],
              goals = [Atom "P" [1 :% []] & mkGoal 0]
            }
        )
        EngineSuccess,
      mkTest_Engine
        "noFreshen"
        ( (defaultConfig @String @Int @String)
            { initialGas = FiniteGas 50,
              rules =
                [ -- R1 will be used on P(1) goal to yield subgoals Q(1) and Q(0), and setting x := 1
                  mkRule
                    "R1"
                    [ Atom "Q" [0 :% []] & GoalHyp . mkHypGoal,
                      Atom "Q" ["x"] & GoalHyp . mkHypGoal
                    ]
                    (Atom "P" ["x"]),
                  -- R2 will be able to solve Q(1) since x := 1, but can't solve Q(0), and so engine will fail
                  mkRule
                    "R2"
                    []
                    (Atom "Q" [VarExpr ("x" {noFreshenVar = True})])
                ],
              goals = [Atom "P" [1 :% []] & mkGoal 0]
            }
        )
        EngineFailure
    ]
