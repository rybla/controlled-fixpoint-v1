{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
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
        EngineSuccess,
      mkTest
        (nat `arr` bool)
        (int `arr` bool)
        EngineFailure
    ]

mkTest :: Expr -> Expr -> EngineResult -> TestTree
mkTest a b r =
  mkTest_Engine
    ( prettyShow a
        <> ( case r of
               EngineSuccess -> "   <:  "
               EngineFailure -> "  !<:  "
               EngineError -> "   !!  "
           )
        <> prettyShow b
    )
    ( Engine.Config
        { initialGas = 100,
          rules = rulesSubtyping,
          goals = [isValid (a `subtype` b)],
          delayable = const False
        }
    )
    r

rulesSubtyping :: [Rule]
rulesSubtyping =
  [ Rule
      { name = "bool <: bool",
        hyps = [],
        conc = isValid (bool `subtype` bool)
      },
    Rule
      { name = "int <: int",
        hyps = [],
        conc = isValid (int `subtype` int)
      },
    Rule
      { name = "nat <: nat",
        hyps = [],
        conc = isValid (nat `subtype` nat)
      },
    Rule
      { name = "nat <: int",
        hyps = [],
        conc = isValid (nat `subtype` int)
      },
    Rule
      { name = "arr",
        hyps =
          [ AtomHyp $ isValid (a' `subtype` a),
            AtomHyp $ isValid (b `subtype` b')
          ],
        conc = isValid ((a `arr` b) `subtype` (a' `arr` b'))
      }
  ]
  where
    (a, a', b, b') = ("a", "a'", "b", "b'")

isValid :: Expr -> Atom
isValid = Atom "isValid"

subtype :: Expr -> Expr -> Expr
subtype a b = ConExpr (Con "subtype" [a, b])

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
