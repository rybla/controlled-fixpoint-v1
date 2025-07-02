{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use ++" #-}

module Spec.Engine.ApplicativeFunctorSubtyping (tests) where

import ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
import qualified Data.Map as Map
import Spec.Engine.Common
  ( EngineResult (..),
    mkTest_Engine,
  )
import Test.Tasty (TestTree, testGroup)
import Text.PrettyPrint (Doc, parens, render, (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Prelude hiding (fmap, pure)

tests :: TestTree
tests =
  testGroup
    "ApplicativeFunctorSubtyping"
    [tests_v1]

tests_v1 :: TestTree
tests_v1 =
  testGroup
    "v1"
    [ mkTest
        (Int :-> Bool)
        (List :@ Int :-> List :@ Bool)
        (fmap functorList (subArrow subInt subBool)),
      mkTest
        (Int :-> Bool)
        (List :@ Nat :-> List :@ Bool)
        (fmap functorList (subArrow subNatOfInt subBool)),
      mkTest
        Bool
        (List :@ Bool)
        (pure functorList subBool),
      mkTest
        Bool
        (List :@ (List :@ (List :@ (List :@ (List :@ Bool)))))
        (pure functorList (pure functorList (pure functorList (pure functorList (pure functorList subBool)))))
    ]
  where
    mkTest s t pf' =
      let pf = "?{s <: t}"
       in mkTest_Engine
            (render $ prettyExpr (s :<: t))
            ( Config
                { goals = [Valid (s :<: t) (VarExpr pf)],
                  rules = rules_v1,
                  initialGas = FiniteGas 50,
                  strategy = DepthFirstStrategy,
                  delayable = \case
                    Valid (Functor (VarExpr _)) _ -> True
                    Valid (VarExpr _ :<: VarExpr _) _ -> True
                    _ -> False
                }
            )
            (EngineSuccessWithSubst $ Subst $ Map.fromList [(pf, pf')])

    rules_v1 :: [Rule]
    rules_v1 =
      concat
        [ -- core rules
          [ Rule
              { name = "subArrow",
                hyps =
                  [ AtomHyp $ Valid (a' :<: a) a'_subtype_a,
                    AtomHyp $ Valid (b :<: b') b_subtype_b'
                  ],
                conc = Valid ((a :-> b) :<: (a' :-> b')) (subArrow a'_subtype_a b_subtype_b')
              },
            Rule
              { name = "subFunctor",
                hyps =
                  [ AtomHyp $ Valid (Functor f) functor_f,
                    AtomHyp $ Valid (a :<: a') a_subtype_a'
                  ],
                conc = Valid (f :@ a :<: f :@ a') (subFunctor functor_f a_subtype_a')
              },
            Rule
              { name = "fmap",
                hyps =
                  [ AtomHyp $ Valid (Functor f) functor_f,
                    AtomHyp $ Valid (a :-> b :<: a' :-> b') a_arrow_a'_subtype_b_arrow_b'
                  ],
                conc = Valid (a :-> b :<: f :@ a' :-> f :@ b') (fmap functor_f a_arrow_a'_subtype_b_arrow_b')
              },
            Rule
              { name = "pure",
                hyps =
                  [ AtomHyp $ Valid (Functor f) functor_f,
                    AtomHyp $ Valid (a :<: a') a_subtype_a'
                  ],
                conc = Valid (a :<: f :@ a') (pure functor_f a_subtype_a')
              }
          ],
          -- datatype rules
          [ Rule
              { name = "subNatOfInt",
                hyps = [],
                conc = Valid (Nat :<: Int) subNatOfInt
              },
            Rule
              { name = "functorList",
                hyps = [],
                conc = Valid (Functor List) functorList
              },
            Rule
              { name = "subBool",
                hyps = [],
                conc = Valid (Bool :<: Bool) subBool
              },
            Rule
              { name = "subInt",
                hyps = [],
                conc = Valid (Int :<: Int) subInt
              },
            Rule
              { name = "subNat",
                hyps = [],
                conc = Valid (Nat :<: Nat) subNat
              }
          ]
        ]
      where
        f = "?f"
        functor_f = "?{Functor f}"

        a = "?a"
        a' = "?a'"
        a'_subtype_a = "?{a' <: a}"
        a_subtype_a' = "?{a <: a'}"

        b = "?b"
        b' = "?b'"
        b_subtype_b' = "?{b <: b'}"

        a_arrow_a'_subtype_b_arrow_b' = "?{a -> a' <: b -> b'}"

subArrow :: Expr -> Expr -> Expr
subArrow a'_subtype_a b_subtype_b' = con "subArrow" [a'_subtype_a, b_subtype_b']

subFunctor :: Expr -> Expr -> Expr
subFunctor functor_f a_subtype_a' = con "subFunctor" [functor_f, a_subtype_a']

fmap :: Expr -> Expr -> Expr
fmap functor_f a_arrow_a'_subtype_b_arrow_b' = con "fmap" [functor_f, a_arrow_a'_subtype_b_arrow_b']

pure :: Expr -> Expr -> Expr
pure functor_f a_subtype_a' = con "pure" [functor_f, a_subtype_a']

subNatOfInt :: Expr
subNatOfInt = con "subNatOfInt" []

functorList :: Expr
functorList = con "functorList" []

subBool :: Expr
subBool = con "subBool" []

subInt :: Expr
subInt = con "subInt" []

subNat :: Expr
subNat = con "subNat" []

prettyExpr :: Expr -> Doc
prettyExpr (s :<: t) = prettyExpr s <+> "<:" <+> prettyExpr t
prettyExpr (Functor f) = "Functor" <+> prettyExpr f
prettyExpr (s :-> t) = parens (prettyExpr s <+> "->" <+> prettyExpr t)
prettyExpr (f :@ t) = parens (prettyExpr f <+> prettyExpr t)
prettyExpr List = "List"
prettyExpr Bool = "Bool"
prettyExpr Nat = "Nat"
prettyExpr Int = "Int"
prettyExpr e = pPrint e

-- atoms

pattern Valid :: Expr -> Expr -> Atom
pattern Valid st pf = Atom "Valid" [st, pf]

-- relations

pattern (:<:) :: Expr -> Expr -> Expr
pattern (:<:) s t = ConExpr (Con "Subtype" [s, t])

infix 4 :<:

pattern Functor :: Expr -> Expr
pattern Functor f = ConExpr (Con "Functor" [f])

-- expressions

pattern (:->) :: Expr -> Expr -> Expr
pattern a :-> b = ConExpr (Con "Arrow" [a, b])

infixr 5 :->

pattern (:@) :: Expr -> Expr -> Expr
pattern f :@ a = ConExpr (Con "App" [f, a])

infixl 6 :@

pattern List :: Expr
pattern List = ConExpr (Con "List" [])

pattern Bool :: Expr
pattern Bool = ConExpr (Con "Bool" [])

pattern Nat :: Expr
pattern Nat = ConExpr (Con "Nat" [])

pattern Int :: Expr
pattern Int = ConExpr (Con "Int" [])
