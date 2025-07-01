{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.Proof (tests) where

import ControlledFixpoint.Engine (Config (initialGas))
import qualified ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
  ( Atom (Atom),
    Con (Con),
    ConName (ConName),
    Expr (ConExpr, VarExpr),
    Hyp (AtomHyp),
    Rule (..),
    RuleName (RuleName),
  )
import Data.Function ((&))
import Spec.Engine.Common
  ( EngineResult (..),
    mkTest_Engine,
  )
import Test.Tasty (TestName, TestTree, testGroup)
import Text.PrettyPrint.HughesPJClass (prettyShow)

tests :: TestTree
tests =
  testGroup
    "Proof"
    [tests_ex1]

tests_ex1 :: TestTree
tests_ex1 =
  testGroup
    "ex1"
    [ mkTest_Equal 0 0 EngineSuccess,
      mkTest_Equal 1 1 EngineSuccess,
      mkTest_Equal 0 1 EngineFailure,
      -- mkTest_Equal (1 + 1) (S (1 + 0)) EngineSuccess,
      -- mkTest_Equal (1 + 1) "result" (EngineSuccessWithSubst (Subst (Map.fromList [("result", S (S Z))])))
      -- mkTest_Equal (1 + 1) 2 EngineFailure,
      -- mkTest_Equal (2 + 3) "result" EngineFailure,
      mkTest_Norm' "norm#1" (0 + 1) EngineError,
      mkTest_Equal 0 0 EngineSuccess
    ]
  where
    mkTest_Equal :: Expr -> Expr -> EngineResult -> TestTree
    mkTest_Equal a b =
      mkTest_Engine
        (prettyShow $ a :== b)
        Engine.Config
          { goals = [Valid (a :== b) "{a == b}", Valid (Norm b) "{Norm b}"],
            rules = rules_ex1,
            delayable = const False,
            initialGas = 100
          }

    mkTest_Norm :: Expr -> EngineResult -> TestTree
    mkTest_Norm input = mkTest_Norm' (prettyShow $ Norm input) input

    mkTest_Norm' :: TestName -> Expr -> EngineResult -> TestTree
    mkTest_Norm' testName input =
      mkTest_Engine
        testName
        Engine.Config
          { goals = [Valid (Norm input) "{Norm input}"],
            rules = rules_ex1,
            delayable = \case
              Valid (VarExpr _ :== VarExpr _) _ -> True
              _ -> False,
            initialGas = 20
          }

    rules_ex1 =
      [ -- compute Add
        let name_ = "{a + Z == Z}"
         in Rule
              { name = RuleName name_,
                hyps = [],
                conc =
                  Valid ("a" + Z :== Z) $
                    ConExpr $
                      Con
                        (ConName name_)
                        ["a"]
              },
        let name_ = "{a + S b == S (a + b)}"
         in Rule
              { name = RuleName name_,
                hyps = [],
                conc =
                  Valid ("a" + S "b" :== S ("a" + "b")) $
                    ConExpr $
                      Con
                        (ConName name_)
                        ["a", "b"]
              },
        -- -- compute Mul
        -- let name_ = "{a * Z == Z}"
        --  in Rule
        --       { name = RuleName name_,
        --         hyps = [],
        --         conc =
        --           Valid ("a" * Z :== Z) $
        --             ConExpr $
        --               Con
        --                 (ConName name_)
        --                 ["a"]
        --       },
        -- let name_ = "{a * (S b) == a + (a * b)}"
        --  in Rule
        --       { name = RuleName name_,
        --         hyps = [],
        --         conc =
        --           Valid ("a" * S "b" :== "a" + ("a" * "b")) $
        --             ConExpr $
        --               Con
        --                 (ConName name_)
        --                 ["a", "b"]
        --       },
        -- -- Distributivity
        -- let name_ = "{a * (b + c) == (a * b) + (a * c)}"
        --  in Rule
        --       { name = RuleName name_,
        --         hyps = [],
        --         conc =
        --           Valid ("a" * ("b" + "c") :== ("a" * "b") + ("a" * "c")) $
        --             ConExpr $
        --               Con
        --                 (ConName name_)
        --                 ["a", "b", "c"]
        --       },
        -- congruences
        let name_ = "{Z == Z}"
         in Rule
              { name = RuleName name_,
                hyps = [],
                conc = Valid (Z :== Z) . ConExpr $ Con (ConName name_) []
              },
        let name_ = "{a == a' |- S a == S a'}"
         in Rule
              { name = RuleName name_,
                hyps = [AtomHyp $ Valid ("a" :== "a'") "{a == a'}"],
                conc =
                  Valid (S "a" :== S "a'") $
                    ConExpr $
                      Con
                        (ConName name_)
                        ["a", "a'", "{a == a'}"]
              },
        let name_ = "{a == a', Norm a |- Norm a'}"
         in Rule
              { name = RuleName name_,
                hyps =
                  [ AtomHyp $ Valid (Norm "a") "{Norm a}",
                    AtomHyp $ Valid ("a" :== "a'") "{a == a'}"
                  ],
                conc =
                  Valid (Norm "a'") $
                    ConExpr $
                      Con
                        (ConName name_)
                        ["a", "a'", "{a == a'}"]
              },
        let name_ = "{a == a', b == b' |- a + b == a + b'}"
         in Rule
              { name = RuleName name_,
                hyps =
                  [ AtomHyp $ Valid ("a" :== "a'") "{a == a'}",
                    AtomHyp $ Valid ("b" :== "b'") "{b == b'}"
                  ],
                conc =
                  Valid ("a" + "b" :== "a" + "b'") . ConExpr $
                    Con (ConName name_) ["a", "a'", "b", "b'", "{a == a'}", "{b == b'}"]
              },
        -- let name_ = "{a == a', b == b' |- a * b == a * b'}"
        --  in Rule
        --       { name = RuleName name_,
        --         hyps =
        --           [ AtomHyp $ Valid ("a" :== "a'") "{a == a'}",
        --             AtomHyp $ Valid ("b" :== "b'") "{b == b'}"
        --           ],
        --         conc =
        --           Valid ("a" * "b" :== "a" * "b'") . ConExpr $
        --             Con (ConName name_) ["a", "a'", "b", "b'", "{a == a'}", "{b == b'}"]
        --       },
        -- -- Transivity
        -- let name_ = "{a == b, b == c |- a == c}"
        --  in Rule
        --       { name = RuleName name_,
        --         hyps =
        --           [ AtomHyp $ Valid ("a" :== "b") "{a == b}",
        --             AtomHyp $ Valid ("b" :== "c") "{b == c}"
        --           ],
        --         conc =
        --           Valid ("a" :== "c") . ConExpr $
        --             Con (ConName name_) ["a", "b", "c", "{a == b}", "{b == c}"]
        --       }
        -- normal forms
        let name_ = "{Norm Z}"
         in Rule
              { name = RuleName name_,
                hyps = [],
                conc =
                  Valid (Norm Z) . ConExpr $
                    Con (ConName name_) []
              },
        let name_ = "{Norm (S a)}"
         in Rule
              { name = RuleName name_,
                hyps =
                  [ AtomHyp $ Valid (Norm "a") "{Norm a}"
                  ],
                conc =
                  Valid (Norm (S "a")) . ConExpr $
                    Con (ConName name_) ["a", "Norm a"]
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
  (*) = (:*)
  abs = undefined
  signum = undefined
  negate = undefined

-- definitions

pattern Tuple :: [Expr] -> Expr
pattern Tuple es = ConExpr (Con "Tuple" es)

pattern Valid :: Expr -> Expr -> Atom
pattern Valid eq pf = Atom "Valid" (Tuple [eq, pf])

pattern (:==) :: Expr -> Expr -> Expr
pattern a :== b = ConExpr (Con "Equal" [a, b])

infix 4 :==

pattern Norm :: Expr -> Expr
pattern Norm a = ConExpr (Con "Norm" [a])

pattern (:+) :: Expr -> Expr -> Expr
pattern a :+ b = ConExpr (Con "Add" [a, b])

infixl 6 :+

pattern (:*) :: Expr -> Expr -> Expr
pattern a :* b = ConExpr (Con "Mul" [a, b])

infixl 6 :*
