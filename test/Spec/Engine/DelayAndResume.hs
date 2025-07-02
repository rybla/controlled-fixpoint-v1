{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.DelayAndResume (tests) where

import ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
import Spec.Engine.Common
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "DelayAndResume"
    [ mkTest_Engine
        "simple delay"
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
            delayable = \case
              VarExpr _ :~ VarExpr _ -> True
              _ -> False,
            goals = ["y" :~ "x", A :~ "y", "x" :~ B]
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
                    hyps = [AtomHyp $ A :~ S "x"],
                    conc = A :~ "x"
                  }
              ],
            delayable = const False,
            goals = [A :~ B]
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
          { goals = [P "x" "y", Q "y" B],
            delayable = \case
              P (VarExpr _) (VarExpr _) -> True
              _ -> False
          }
        EngineSuccess,
      mkTest_Engine
        "exhaust terminating branch"
        cfg
          { goals = [P "x" "y", Q "y" B]
          }
        (EngineError OutOfGas)
    ]
  where
    cfg :: Config
    cfg =
      Engine.Config
        { initialGas = FiniteGas 10,
          strategy = DepthFirstStrategy,
          rules = rules1,
          delayable = const False,
          goals = []
        }

    rules1 :: [Rule]
    rules1 =
      [ Rule
          { name = "R1",
            hyps = [AtomHyp $ P (S "x") A],
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

pattern (:~) :: Expr -> Expr -> Atom
pattern x :~ y = Atom "Valid" (ConExpr (Con "Rel" [x, y]))

pattern P :: Expr -> Expr -> Atom
pattern P x y = Atom "Valid" (ConExpr (Con "P" [x, y]))

pattern Q :: Expr -> Expr -> Atom
pattern Q x y = Atom "Valid" (ConExpr (Con "Q" [x, y]))

pattern A :: Expr
pattern A = ConExpr (Con "A" [])

pattern B :: Expr
pattern B = ConExpr (Con "B" [])

pattern S :: Expr -> Expr
pattern S x = ConExpr (Con "S" [x])
