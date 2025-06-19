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
          goals = [a `subtype` b],
          delayable = const False
        }
    )
    r

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
      { name = "arr",
        hyps =
          [ AtomHyp $ a' `subtype` a,
            AtomHyp $ b `subtype` b'
          ],
        conc = (a `arr` b) `subtype` (a' `arr` b')
      },
    Rule
      { name = "map",
        hyps =
          [ AtomHyp $ (a `arr` b) `subtype` (a' `arr` b'),
            AtomHyp $ functor f
          ],
        conc = (a `arr` b) `subtype` ((f `app` a) `arr` (f `app` b))
      }
  ]
  where
    (f, a, a', b, b') = ("f", "a", "a'", "b", "b'")

-- atoms

subtype :: Expr -> Expr -> Atom
subtype a b = Atom "subtype" $ ConExpr (Con "subtype" [a, b])

functor :: Expr -> Atom
functor f = Atom "functor" $ ConExpr (Con "functor" [f])

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

app :: Expr -> Expr -> Expr
app f a = ConExpr (Con "app" [f, a])
