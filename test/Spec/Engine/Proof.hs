{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.Proof (tests) where

import ControlledFixpoint.Engine (Config (initialGas))
import qualified ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
import Data.Function ((&))
import qualified Data.Map as Map
import Spec.Engine.Common
  ( EngineResult (..),
    mkTest_Engine,
  )
import Test.Tasty (TestTree, testGroup)
import Text.PrettyPrint.HughesPJClass (prettyShow)

tests :: TestTree
tests =
  testGroup
    "Proof"
    [tests_NormV1]

tests_NormV1 :: TestTree
tests_NormV1 =
  testGroup
    "NormV1"
    [ mkTest 0 $ EngineSuccessWithSubst $ Subst $ Map.fromList [("?output", 0)],
      mkTest 1 $ EngineSuccessWithSubst $ Subst $ Map.fromList [("?output", 1)],
      mkTest (0 + 0) EngineError
      -- mkTest (1 + 1) $ EngineSuccessWithSubst $ Subst $ Map.fromList [("?output", 2)]
      -- mkTest (1 + 1) EngineError
    ]
  where
    mkTest :: Expr -> EngineResult -> TestTree
    mkTest input =
      mkTest_Engine
        (prettyShow input)
        Engine.Config
          { goals = [Valid (input :⇓ "?output") "?{input ⇓ output}"],
            rules = rules,
            delayable = \case
              Valid (VarExpr _ :⇓ VarExpr _) _ -> True
              _ -> False,
            initialGas = 4
          }

    rules =
      [ -- normal forms
        let name_ = "{Z ⇓ ...}"
         in Rule
              { name = RuleName name_,
                hyps = [],
                conc = Valid (Z :⇓ Z) . ConExpr $ Con (ConName name_) []
              },
        let name_ = "{S a ⇓ ...}"
            (a, a', pf_a_norm_a') = ("?a", "?a'", "?{a ⇓ a'}")
         in Rule
              { name = RuleName name_,
                hyps = [AtomHyp $ Valid (a :⇓ a') pf_a_norm_a'],
                conc = Valid (S a :⇓ S a') . ConExpr $ Con (ConName name_) [a, a', pf_a_norm_a']
              },
        let name_ = "{a + Z ⇓ ...}"
            (a, a', pf_a_norm_a') = ("?a", "?a'", "?{a ⇓ a'}")
         in Rule
              { name = RuleName name_,
                hyps =
                  [ AtomHyp $ Valid (a :⇓ a') pf_a_norm_a'
                  ],
                conc = Valid (a + 0 :⇓ a') . ConExpr $ Con (ConName name_) [a, a', pf_a_norm_a']
              },
        let name_ = "{a + S b ⇓ ...}"
            (a, a', pf_a_norm_a', b, b', pf_b_norm_Sb', c, pf_a'_plus_b'_norm_c) = ("?a", "?a'", "?{a ⇓ a'}", "?b", "?b'", "?{b ⇓ S b'}", "?c", "?{a' + b' ⇓ c}")
         in Rule
              { name = RuleName name_,
                hyps =
                  [ AtomHyp $ Valid (a :⇓ a') pf_a_norm_a',
                    AtomHyp $ Valid (b :⇓ S b') pf_b_norm_Sb',
                    AtomHyp $ Valid (a' + b' :⇓ c) pf_a'_plus_b'_norm_c
                  ],
                conc = Valid (a + b :⇓ S c) . ConExpr $ Con (ConName name_) [a, a', pf_a_norm_a', b, b', c, pf_b_norm_Sb']
              }
      ]

-- nat

pattern Z :: Expr
pattern Z = ConExpr (Con "Z" [])

pattern S :: Expr -> Expr
pattern S n = ConExpr (Con "S" [n])

instance Num Expr where
  fromInteger n = replicate (fromInteger n) S & foldr ($) Z
  (+) = (:+)

-- definitions

pattern Tuple :: [Expr] -> Expr
pattern Tuple es = ConExpr (Con "Tuple" es)

pattern Valid :: Expr -> Expr -> Atom
pattern Valid eq pf = Atom "Valid" (Tuple [eq, pf])

pattern (:⇓) :: Expr -> Expr -> Expr
pattern (:⇓) a b = ConExpr (Con "Norm" [a, b])

infix 4 :⇓

pattern (:+) :: Expr -> Expr -> Expr
pattern a :+ b = ConExpr (Con "Add" [a, b])

infixl 6 :+
