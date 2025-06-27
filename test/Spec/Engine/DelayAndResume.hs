{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.DelayAndResume (tests) where

import ControlledFixpoint.Engine (Config (initialGas))
import qualified ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
import Spec.Engine.Common
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "DelayAndResume"
    [ mkTest_Engine
        "simple"
        Engine.Config
          { initialGas = 100,
            rules =
              [ Rule
                  { name = "A ~ B",
                    hyps = [],
                    conc = Rel A B
                  },
                Rule
                  { name = "B ~ A",
                    hyps = [],
                    conc = Rel B A
                  }
              ],
            delayable = \case
              Rel (VarExpr _) (VarExpr _) -> True
              _ -> False,
            goals = [Rel "y" "x", Rel A "y", Rel "x" B]
          }
        EngineSuccess,
      unrolling_tests
    ]

unrolling_tests :: TestTree
unrolling_tests =
  testGroup
    "unrolling"
    [ mkTest_Engine
        "avoid nonterminating branch"
        config
          { Engine.goals = [P "x" "y", Q "y" B],
            Engine.delayable = \case
              P (VarExpr _) (VarExpr _) -> True
              _ -> False
          }
        EngineSuccess,
      mkTest_Engine
        "exhaust terminating branch"
        config
          { Engine.goals = [P "x" "y", Q "y" B]
          }
        EngineSuccess
    ]
  where
    config :: Config
    config =
      Engine.Config
        { initialGas = 100,
          rules =
            [ Rule
                { name = "P (S x) A  |-  P x A",
                  hyps = [AtomHyp $ P (S "x") A],
                  conc = P "x" A
                },
              Rule
                { name = "P B B",
                  hyps = [],
                  conc = P B B
                },
              Rule
                { name = "Q B B",
                  hyps = [],
                  conc = Q B B
                }
            ],
          delayable = const False,
          goals = []
        }

pattern Rel :: Expr -> Expr -> Atom
pattern Rel x y = Atom "atom" (ConExpr (Con "rel" [x, y]))

pattern A :: Expr
pattern A = ConExpr (Con "A" [])

pattern B :: Expr
pattern B = ConExpr (Con "B" [])

pattern S :: Expr -> Expr
pattern S x = ConExpr (Con "S" [x])

pattern P :: Expr -> Expr -> Atom
pattern P x y = Atom "atom" (ConExpr (Con "P" [x, y]))

pattern Q :: Expr -> Expr -> Atom
pattern Q x y = Atom "atom" (ConExpr (Con "Q" [x, y]))
