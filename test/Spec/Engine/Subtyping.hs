{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.Subtyping (tests) where

import qualified ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
import Data.String (IsString (fromString))
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
        (EngineSuccess Nothing),
      mkTest
        (nat `arr` bool)
        (int `arr` bool)
        (EngineFailure Nothing),
      mkTest
        "x"
        ("y" `arr` "z")
        (EngineSuccessWithDelays Nothing)
    ]

mkTest :: Expr -> Expr -> EngineResult -> TestTree
mkTest a b =
  mkTest_Engine
    ("`" <> displayExpr a <> "  <:  " <> displayExpr b <> "`")
    ( Engine.Config
        { initialGas = 100,
          rules = rulesSubtyping,
          goals = [a `subtype` b],
          delayable = \case
            Atom _ (ConExpr (Con "subtype" [VarExpr _, VarExpr _])) -> True
            _ -> False
        }
    )

rulesSubtyping :: [Rule]
rulesSubtyping =
  [ Rule
      { name = "bool <: bool",
        hyps = [],
        conc = bool `subtype` bool
      },
    Rule
      { name = "int <: int",
        hyps = [],
        conc = int `subtype` int
      },
    Rule
      { name = "nat <: nat",
        hyps = [],
        conc = nat `subtype` nat
      },
    Rule
      { name = "nat <: int",
        hyps = [],
        conc = nat `subtype` int
      },
    Rule
      { name = "a' <: a , b <: b'  |-  a -> b <: a' -> b'",
        hyps =
          [ AtomHyp $ a' `subtype` a,
            AtomHyp $ b `subtype` b'
          ],
        conc = (a `arr` b) `subtype` (a' `arr` b')
      }
  ]
  where
    (a, a', b, b') = ("a", "a'", "b", "b'")

-- atoms

subtype :: Expr -> Expr -> Atom
subtype a b = Atom "subtype" $ ConExpr (Con "subtype" [a, b])

-- expressions

var :: String -> Expr
var x = VarExpr (Var x Nothing)

instance IsString Expr where fromString = var

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
displayExpr (ConExpr (Con "arr" [a, b])) = displayExpr a <> " -> " <> displayExpr b
displayExpr (VarExpr (Var x _)) = x
displayExpr e = prettyShow e
