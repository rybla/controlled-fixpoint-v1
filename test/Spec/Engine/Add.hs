{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Spec.Engine.Add (tests) where

import qualified ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
import Data.String (IsString (fromString))
import Spec.Engine.Common
import Test.Tasty (TestTree, testGroup)
import Text.PrettyPrint.HughesPJClass (prettyShow)

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

mkTest :: Expr -> Expr -> Expr -> EngineResult -> TestTree
mkTest a b c =
  mkTest_Engine (prettyShow goal) $
    Engine.Config
      { initialGas = 100,
        rules = rules_add,
        goals = [goal]
      }
  where
    goal = isTrue (a +. b ==. c)

rules_add :: [Rule]
rules_add =
  [ Rule
      { name = RuleName $ show @String "0 + x = x",
        hyps = [],
        conc = Atom "IsTrue" (0 + "x" ==. "x")
      },
    Rule
      { name = RuleName $ show @String "x + y = z ==> suc x + y = suc z",
        hyps =
          [AtomHyp $ Atom "IsTrue" ("x" +. "y" ==. "z")],
        conc =
          Atom "IsTrue" (sucExpr "x" +. "y" ==. sucExpr "z")
      }
  ]

isTrue :: Expr -> Atom
isTrue = Atom "IsTrue"

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
