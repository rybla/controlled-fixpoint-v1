{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.Proof (tests) where

import qualified ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
import Data.Function ((&))
import qualified Data.Map as Map
import Spec.Engine.Common
  ( EngineResult (..),
    mkTest_Engine,
  )
import Test.Tasty (TestName, TestTree, testGroup)
import Text.PrettyPrint (Doc, parens, text)
import Text.PrettyPrint.HughesPJClass (pPrint, render, (<+>))

tests :: TestTree
tests =
  testGroup
    "Proof"
    [tests_NormV1]

tests_NormV1 :: TestTree
tests_NormV1 =
  testGroup
    "NormV1"
    [ mkTest_success 0,
      mkTest_success 1,
      mkTest_success (0 + 0),
      mkTest_success (1 + 0),
      mkTest_success (1 + 1),
      mkTest_success (0 + 2),
      mkTest_success (2 + 0),
      mkTest_success (3 + 4),
      mkTest_successWithProof 0 pZ,
      mkTest_successWithProof 1 (pS pZ),
      mkTest_successWithProof (0 + 0) (pPZ pZ pZ),
      mkTest_successWithProof (1 + 0) (pPZ (pS pZ) pZ),
      mkTest_successWithProof (1 + 1) (pPS (pS pZ) (pS pZ) (pPZ (pS pZ) pZ)),
      mkTest_successWithProof (0 + 2) (pPS pZ (pS (pS pZ)) (pPS pZ (pS pZ) (pPZ pZ pZ))),
      mkTest_successWithProof (2 + 0) (pPZ (pS (pS pZ)) pZ),
      mkTest_successWithProof (3 + 4) (pPS (pS (pS (pS pZ))) (pS (pS (pS (pS pZ)))) (pPS (pS (pS (pS pZ))) (pS (pS (pS pZ))) (pPS (pS (pS (pS pZ))) (pS (pS pZ)) (pPS (pS (pS (pS pZ))) (pS pZ) (pPZ (pS (pS (pS pZ))) pZ)))))
    ]
  where
    pZ = con "Z⇓" []
    pS a = con "S⇓" [a]
    pPZ a b = con "+Z⇓" [a, b]
    pPS a b c = con "+S⇓" [a, b, c]

    mkTest_success :: Expr -> TestTree
    mkTest_success x =
      mkTest (render (prettyExpr x <+> "⇓" <+> text (show y))) x $
        EngineSuccessWithSubst
          (Subst $ Map.fromList [("?out", fromIntegral y)])
      where
        y = interpret x

    mkTest_successWithProof :: Expr -> Expr -> TestTree
    mkTest_successWithProof x pf =
      mkTest (render (prettyExpr x <+> "⇓" <+> text (show y) <+> "via proof")) x $
        EngineSuccessWithSubst
          (Subst $ Map.fromList [("?out", fromIntegral y), ("?{in ⇓ out}", pf)])
      where
        y = interpret x

    mkTest :: TestName -> Expr -> EngineResult -> TestTree
    mkTest testName in_ =
      mkTest_Engine
        testName
        Engine.Config
          { goals = [Valid (in_ :⇓ "?out") "?{in ⇓ out}"],
            rules = rules,
            delayable = \case
              Valid (VarExpr _ :⇓ VarExpr _) _ -> True
              _ -> False,
            initialGas = 100
          }

    rules =
      [ -- normal forms
        let name_ = "Z⇓"
         in Rule
              { name = RuleName name_,
                hyps = [],
                conc = Valid (Z :⇓ Z) . ConExpr $ Con (ConName name_) []
              },
        let name_ = "S⇓"
            (a, a', pf_a_norm_a') = ("?a", "?a'", "?{a ⇓ a'}")
         in Rule
              { name = RuleName name_,
                hyps = [AtomHyp $ Valid (a :⇓ a') pf_a_norm_a'],
                conc = Valid (S a :⇓ S a') . ConExpr $ Con (ConName name_) [pf_a_norm_a']
              },
        let name_ = "+Z⇓"
            (a, a', pf_a_norm_a', b, pf_b_norm_Z) = ("?a", "?a'", "?{a ⇓ a'}", "b", "?{b ⇓ Z}")
         in Rule
              { name = RuleName name_,
                hyps =
                  [ AtomHyp $ Valid (a :⇓ a') pf_a_norm_a',
                    AtomHyp $ Valid (b :⇓ Z) pf_b_norm_Z
                  ],
                conc = Valid (a + b :⇓ a') . ConExpr $ Con (ConName name_) [pf_a_norm_a', pf_b_norm_Z]
              },
        let name_ = "+S⇓"
            (a, a', pf_a_norm_a', b, b', pf_b_norm_Sb', c, pf_a'_plus_b'_norm_c) = ("?a", "?a'", "?{a ⇓ a'}", "?b", "?b'", "?{b ⇓ S b'}", "?c", "?{a' + b' ⇓ c}")
         in Rule
              { name = RuleName name_,
                hyps =
                  [ AtomHyp $ Valid (a :⇓ a') pf_a_norm_a',
                    AtomHyp $ Valid (b :⇓ S b') pf_b_norm_Sb',
                    AtomHyp $ Valid (a' + b' :⇓ c) pf_a'_plus_b'_norm_c
                  ],
                conc = Valid (a + b :⇓ S c) . ConExpr $ Con (ConName name_) [pf_a_norm_a', pf_b_norm_Sb', pf_a'_plus_b'_norm_c]
              }
      ]

interpret :: Expr -> Int
interpret Z = 0
interpret (S n) = 1 + interpret n
interpret (a :+ b) = interpret a + interpret b
interpret e = error . render $ "interpret" <+> pPrint e

prettyExpr :: Expr -> Doc
prettyExpr e | Just i <- toInt e = text (show i)
prettyExpr (S a) = parens ("S" <+> prettyExpr a)
prettyExpr (a :+ b) = prettyExpr a <+> "+" <+> prettyExpr b
prettyExpr e = error . render $ "prettyExpr" <+> pPrint e

toInt :: Expr -> Maybe Int
toInt Z = Just 0
toInt (S a) = do
  i <- toInt a
  Just (1 + i)
toInt _ = Nothing

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
pattern Valid st pf = Atom "Valid" (Tuple [st, pf])

pattern (:⇓) :: Expr -> Expr -> Expr
pattern (:⇓) a b = ConExpr (Con "Norm" [a, b])

infix 4 :⇓

pattern (:+) :: Expr -> Expr -> Expr
pattern a :+ b = ConExpr (Con "Add" [a, b])

infixl 6 :+
