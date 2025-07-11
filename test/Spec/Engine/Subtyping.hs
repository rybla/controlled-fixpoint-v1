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

mkTest :: Expr -> Expr -> EngineResult -> TestTree
mkTest a b =
  mkTest_Engine
    ("`" <> displayExpr a <> "  <:  " <> displayExpr b <> "`")
    ( Engine.Config
        { initialGas = FiniteGas 50,
          strategy = DepthFirstStrategy,
          rules = rulesSubtyping,
          exprAliases = [],
          goals = [a :<: b],
          shouldSuspend = \case
            VarExpr _ :<: VarExpr _ -> True
            _ -> False
        }
    )

rulesSubtyping :: [Rule]
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
          [ AtomHyp $ a' :<: a,
            AtomHyp $ b :<: b'
          ],
        conc = (a `arr` b) :<: (a' `arr` b')
      }
  ]
  where
    (a, a', b, b') = ("a", "a'", "b", "b'")

-- atoms

pattern (:<:) :: Expr -> Expr -> Atom
pattern (:<:) s t = Atom "Subtype" [s, t]

-- expressions

int :: Expr
int = ConExpr (Con "int" [])

nat :: Expr
nat = ConExpr (Con "nat" [])

bool :: Expr
bool = ConExpr (Con "bool" [])

arr :: Expr -> Expr -> Expr
arr a b = ConExpr (Con "arr" [a, b])

displayExpr :: Expr -> String
displayExpr (ConExpr (Con "int" [])) = "int"
displayExpr (ConExpr (Con "nat" [])) = "nat"
displayExpr (ConExpr (Con "bool" [])) = "bool"
displayExpr (ConExpr (Con "arr" [a, b])) = displayExpr a <> " → " <> displayExpr b
displayExpr (VarExpr (Var x _)) = x
displayExpr e = prettyShow e
