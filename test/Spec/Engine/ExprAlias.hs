{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.ExprAlias (tests) where

import ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar as Grammar
import qualified Data.Map as Map
import Spec.Engine.Common
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "ExprAlias"
    [ex1_tests]

ex1_tests :: TestTree
ex1_tests =
  testGroup
    "ex1"
    [ mkTest_Engine
        "preserve alias"
        cfg
          { goals = [P (Swap (Tuple A B))],
            rules =
              [ Rule
                  { name = "R1",
                    conc = P "x",
                    hyps = []
                  }
              ]
          }
        ( EngineSuccessWithSubst . Subst . Map.fromList $
            [ (Var "x" (Just 1), Swap (Tuple A B))
            ]
        ),
      mkTest_Engine
        "unfold alias"
        cfg
          { goals = [P (Swap (Tuple A B))],
            rules =
              [ Rule
                  { name = "R1",
                    conc = P (Tuple "x" "y"),
                    hyps = []
                  }
              ]
          }
        ( EngineSuccessWithSubst . Subst . Map.fromList $
            [ (Var "x" (Just 1), B),
              (Var "y" (Just 2), A)
            ]
        )
    ]
  where
    cfg =
      Engine.Config
        { initialGas = FiniteGas 50,
          rules = [],
          goals = [],
          shouldSuspend = const False,
          exprAliases =
            [ ExprAlias \case
                Swap (Tuple x y) -> Just $ Tuple y x
                _ -> Nothing
            ],
          strategy = DepthFirstStrategy
        }

pattern A :: Expr
pattern A = "A" :% []

pattern B :: Expr
pattern B = "B" :% []

pattern P :: Expr -> Atom
pattern P x = Atom "P" [x]

pattern Tuple :: Expr -> Expr -> Expr
pattern Tuple x y = "tuple" :% [x, y]

pattern Swap :: Expr -> Expr
pattern Swap t = "App" :% ["swap" :% [], t]
