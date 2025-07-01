{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.ApplicativeFunctorSubtyping (tests) where

import ControlledFixpoint.Engine
import ControlledFixpoint.Grammar
import Data.Function ((&))
import qualified Data.Map as Map
import Spec.Engine.Common
  ( EngineResult (..),
    mkTest_Engine,
  )
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Text.PrettyPrint (Doc, parens, text)
import Text.PrettyPrint.HughesPJClass (pPrint, render, (<+>))

tests :: TestTree
tests =
  testGroup
    "ApplicativeFunctorSubtyping"
    [ testCase "test#1" (return ()),
      testCase "simple" (return ())
    ]

rules :: [Rule]
rules =
  [ Rule
      { name = "fun-sub"
      },
    Rule
      { name = "fmap"
      },
    Rule
      { name = "pure"
      },
    Rule
      { name = "func",
        hyps = [],
        conc = Valid _ _
      }
  ]

-- definitions

pattern Tuple :: [Expr] -> Expr
pattern Tuple es = ConExpr (Con "Tuple" es)

pattern Valid :: Expr -> Expr -> Atom
pattern Valid st pf = Atom "Valid" (Tuple [st, pf])

pattern (:<:) :: Expr -> Expr -> Expr
pattern (:<:) s t = ConExpr (Con "Subtype" [s, t])

infix 4 :<:

pattern Functor :: Expr -> Expr
pattern Functor e = ConExpr (Con "Functor" [e])

pattern (:->) :: Expr -> Expr -> Expr
pattern a :-> b = ConExpr (Con "Arrow" [a, b])

pattern List :: Expr -> Expr
pattern List a = ConExpr (Con "List" [a])

pattern Bool :: Expr
pattern Bool = ConExpr (Con "Bool" [])

pattern Int :: Expr
pattern Int = ConExpr (Con "Int" [])

pattern Real :: Expr
pattern Real = ConExpr (Con "Real" [])
