{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.Add (tests) where

import qualified ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
import Data.String (IsString (fromString))
import Spec.Engine.Common
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Add"
    [ mkTest 0 0 0 (EngineSuccess Nothing),
      mkTest 0 1 1 (EngineSuccess Nothing),
      mkTest 1 0 1 (EngineSuccess Nothing),
      mkTest 1 2 3 (EngineSuccess Nothing),
      mkTest 1 2 2 (EngineFailure Nothing)
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
          isTrue (sucExpr "x" +. "y" ==. sucExpr "z")
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

varExpr :: String -> Expr
varExpr x = VarExpr (Var x Nothing)

sucExpr :: Expr -> Expr
sucExpr x = ConExpr (Con "suc" [x])

zeroExpr :: Expr
zeroExpr = ConExpr (Con "zero" [])

instance IsString Expr where fromString = varExpr

instance Num Expr where
  (+) = (+.)
  (*) = undefined
  fromInteger n | n < 0 = undefined
  fromInteger 0 = zeroExpr
  fromInteger n = sucExpr (fromInteger (n - 1))

  abs = undefined
  signum = undefined
  negate = undefined
