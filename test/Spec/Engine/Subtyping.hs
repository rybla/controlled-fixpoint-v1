{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.Subtyping (tests) where

import ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
import Spec.Engine.Common
import Test.Tasty (TestTree, testGroup)
import Text.PrettyPrint.HughesPJClass (prettyShow)

tests :: TestTree
tests =
  testGroup
    "Subtyping"
    [ mkTest
        (int `arr` bool)
        (nat `arr` bool)
        EngineSuccess,
      mkTest
        (nat `arr` bool)
        (int `arr` bool)
        EngineFailure,
      mkTest
        "x"
        ("y" `arr` "z")
        EngineSuccessWithSuspends,
      mkTest
        ("x" `arr` "y")
        ("z" `arr` "w")
        EngineSuccessWithSuspends
    ]

mkTest :: Expr C V -> Expr C V -> EngineResult C V -> TestTree
mkTest a b =
  mkTest_Engine
    ("`" <> displayExpr a <> "  <:  " <> displayExpr b <> "`")
    ( Engine.Config
        { initialGas = FiniteGas 50,
          strategy = DepthFirstStrategy,
          rules = rulesSubtyping,
          exprAliases = [],
          goals = [mkGoal $ a :<: b],
          shouldSuspend = \case
            VarExpr _ :<: VarExpr _ -> True
            _ -> False
        }
    )

rulesSubtyping :: [Rule A C V]
rulesSubtyping =
  [ Rule
      { name = "bool <: bool",
        hyps = [],
        conc = bool :<: bool
      },
    Rule
      { name = "int <: int",
        hyps = [],
        conc = int :<: int
      },
    Rule
      { name = "nat <: nat",
        hyps = [],
        conc = nat :<: nat
      },
    Rule
      { name = "nat <: int",
        hyps = [],
        conc = nat :<: int
      },
    Rule
      { name = "a' <: a , b <: b'  ⊢  a → b <: a' → b'",
        hyps =
          [ GoalHyp . mkGoal $ a' :<: a,
            GoalHyp . mkGoal $ b :<: b'
          ],
        conc = (a `arr` b) :<: (a' `arr` b')
      }
  ]
  where
    (a, a', b, b') = ("a", "a'", "b", "b'")

-- atoms

pattern (:<:) :: Expr C V -> Expr C V -> Atom A C V
pattern (:<:) s t = Atom "Subtype" [s, t]

-- expressions

int :: Expr C V
int = ConExpr (Con "int" [])

nat :: Expr C V
nat = ConExpr (Con "nat" [])

bool :: Expr C V
bool = ConExpr (Con "bool" [])

arr :: Expr C V -> Expr C V -> Expr C V
arr a b = ConExpr (Con "arr" [a, b])

displayExpr :: Expr C V -> String
displayExpr (ConExpr (Con "int" [])) = "int"
displayExpr (ConExpr (Con "nat" [])) = "nat"
displayExpr (ConExpr (Con "bool" [])) = "bool"
displayExpr (ConExpr (Con "arr" [a, b])) = displayExpr a <> " → " <> displayExpr b
displayExpr (VarExpr (Var x _)) = x
displayExpr e = prettyShow e
