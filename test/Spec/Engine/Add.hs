{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.Add (tests) where

import qualified ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
import Spec.Engine.Common
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Add"
    [ mkTest 0 0 0 EngineSuccess,
      mkTest 0 1 1 EngineSuccess,
      mkTest 1 0 1 EngineSuccess,
      mkTest 1 2 3 EngineSuccess,
      mkTest 1 2 2 EngineFailure
    ]

mkTest :: Int -> Int -> Int -> EngineResult -> TestTree
mkTest a b c =
  mkTest_Engine
    ("`" <> show a <> " + " <> show b <> " = " <> show c <> "`")
    ( Engine.Config
        { initialGas = 100,
          rules = rulesAdd,
          goals = [goal],
          delayable = const False
        }
    )
  where
    goal = isTrue (fromIntegral a +. fromIntegral b ==. fromIntegral c)

rulesAdd :: [Rule]
rulesAdd =
  [ Rule
      { name = RuleName $ show @String "0 + x = x",
        hyps = [],
        conc = isTrue (0 + "x" ==. "x")
      },
    Rule
      { name = RuleName $ show @String "x + y = z ==> suc x + y = suc z",
        hyps =
          [AtomHyp $ isTrue ("x" +. "y" ==. "z")],
        conc =
          isTrue (suc "x" +. "y" ==. suc "z")
      }
  ]

isTrue :: Expr -> Atom
isTrue = Atom "isTrue"

(==.) :: Expr -> Expr -> Expr
x ==. y = ConExpr (Con "equal" [x, y])

infix 4 ==.

(+.) :: Expr -> Expr -> Expr
x +. y = ConExpr (Con "add" [x, y])

infixl 6 +.

suc :: Expr -> Expr
suc x = ConExpr (Con "suc" [x])

zero :: Expr
zero = ConExpr (Con "zero" [])

instance Num Expr where
  (+) = (+.)
  (*) = undefined
  fromInteger n | n < 0 = undefined
  fromInteger 0 = zero
  fromInteger n = suc (fromInteger (n - 1))

  abs = undefined
  signum = undefined
  negate = undefined
