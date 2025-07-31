{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.SuspendAndResume (tests) where

import ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
import Spec.Engine.Common
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "SuspendAndResume"
    [ mkTest_Engine
        "simple suspend"
        Engine.Config
          { initialGas = FiniteGas 50,
            strategy = DepthFirstStrategy,
            rules =
              [ Rule
                  { name = "R1",
                    hyps = [],
                    conc = A :~ B
                  },
                Rule
                  { name = "R1",
                    hyps = [],
                    conc = B :~ A
                  }
              ],
            exprAliases = [],
            shouldSuspend = \case
              VarExpr _ :~ VarExpr _ -> True
              _ -> False,
            goals =
              [ mkGoal 0 $ "y" :~ "x",
                mkGoal 1 $ A :~ "y",
                mkGoal 2 $ "x" :~ B
              ]
          }
        EngineSuccess,
      mkTest_Engine
        "simple nonterminating"
        Engine.Config
          { initialGas = FiniteGas 10,
            strategy = DepthFirstStrategy,
            rules =
              [ Rule
                  { name = "R1",
                    hyps = [GoalHyp . mkHypGoal $ A :~ S "x"],
                    conc = A :~ "x"
                  }
              ],
            exprAliases = [],
            shouldSuspend = const False,
            goals = [mkGoal 0 $ A :~ B]
          }
        (EngineError OutOfGas),
      unrolling_tests
    ]

unrolling_tests :: TestTree
unrolling_tests =
  testGroup
    "unrolling"
    [ mkTest_Engine
        "avoid nonterminating branch"
        cfg
          { goals = [mkGoal 0  $ P "x" "y", mkGoal 1 $ Q "y" B],
            shouldSuspend = \case
              P (VarExpr _) (VarExpr _) -> True
              _ -> False
          }
        EngineSuccess,
      mkTest_Engine
        "exhaust terminating branch"
        cfg
          { goals = [mkGoal 0  $ P "x" "y", mkGoal 1  $ Q "y" B]
          }
        (EngineError OutOfGas)
    ]
  where
    cfg :: Config A C V
    cfg =
      Engine.Config
        { initialGas = FiniteGas 10,
          strategy = DepthFirstStrategy,
          rules = rules1,
          exprAliases = [],
          shouldSuspend = const False,
          goals = []
        }

    rules1 :: [Rule A C V]
    rules1 =
      [ Rule
          { name = "R1",
            hyps = [GoalHyp . mkHypGoal $ P (S "x") A],
            conc = P "x" A
          },
        Rule
          { name = "R2",
            hyps = [],
            conc = P B B
          },
        Rule
          { name = "R3",
            hyps = [],
            conc = Q B B
          }
      ]

pattern Valid :: Expr C V -> Atom A C V
pattern Valid st = Atom "Valid" [st]

pattern (:~) :: Expr C V -> Expr C V -> Atom A C V
pattern x :~ y = Valid (ConExpr (Con "Rel" [x, y]))

pattern P :: Expr C V -> Expr C V -> Atom A C V
pattern P x y = Valid (ConExpr (Con "P" [x, y]))

pattern Q :: Expr C V -> Expr C V -> Atom A C V
pattern Q x y = Valid (ConExpr (Con "Q" [x, y]))

pattern A :: Expr C V
pattern A = ConExpr (Con "A" [])

pattern B :: Expr C V
pattern B = ConExpr (Con "B" [])

pattern S :: Expr C V -> Expr C V
pattern S x = ConExpr (Con "S" [x])
