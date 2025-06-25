{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.SolutionsCount (tests) where

import ControlledFixpoint.Engine (Config (initialGas))
import qualified ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
import Spec.Engine.Common
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "SolutionsCount"
    [ mkTest_Engine
        "Ex1"
        ( Engine.Config
            { initialGas = 100,
              rules =
                [ Rule
                    { name = "x ~ x",
                      hyps = [],
                      conc = Rel "x" "x"
                    },
                  Rule
                    { name = "A ~ B",
                      hyps = [],
                      conc = Rel A B
                    }
                ],
              delayable = \case
                Rel (VarExpr _) (VarExpr _) -> True
                _ -> False,
              goals = [Rel "y" "x", Rel A "y", Rel "x" B]
            }
        )
        (EngineSuccess Nothing)
    ]

pattern Rel :: Expr -> Expr -> Atom
pattern Rel x y = Atom "atom" (ConExpr (Con "rel" [x, y]))

pattern A :: Expr
pattern A = ConExpr (Con "A" [])

pattern B :: Expr
pattern B = ConExpr (Con "B" [])
