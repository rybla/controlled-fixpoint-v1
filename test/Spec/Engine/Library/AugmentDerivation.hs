{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.Library.AugmentDerivation (tests) where

import ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
import ControlledFixpoint.Library.AugmentDerivation as AugmentDerivation
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.String (IsString (fromString))
import Spec.Engine.Common hiding (goldenDirpath)
import qualified Spec.Engine.Library.Common as Common
import System.FilePath ((<.>), (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Text.PrettyPrint (hang, render, vcat)
import Text.PrettyPrint.HughesPJClass (Pretty (..), prettyShow)

goldenDirpath :: FilePath
goldenDirpath = Common.goldenDirpath </> "AugmentDerivation"

tests :: TestTree
tests =
  testGroup
    "AugmentDerivation"
    [ testGroup
        "rules"
        [ goldenVsString
            "0_hypotheses"
            (goldenDirpath </> "rules" </> "0_hypotheses" <.> "golden")
            do
              return . fromString . prettyShow $
                [ Rule
                    { name = "R1",
                      hyps = [],
                      conc = Q
                    } ::
                    Rule A C V
                ]
                  <&> augmentDerivation_Rule
                    AugmentDerivation.Config {isDerivation = \a -> Just ("?" <> a)},
          goldenVsString
            "1_hypothesis"
            (goldenDirpath </> "rules" </> "1_hypothesis" <.> "golden")
            do
              return . fromString . prettyShow $
                [ Rule
                    { name = "R1",
                      hyps = [GoalHyp . mkGoal $ P1],
                      conc = Q
                    }
                ]
                  <&> augmentDerivation_Rule
                    AugmentDerivation.Config {isDerivation = \a -> Just ("?" <> coerce a)},
          goldenVsString
            "3_hypotheses"
            (goldenDirpath </> "rules" </> "3_hypotheses" <.> "golden")
            do
              return . fromString . prettyShow $
                [ Rule
                    { name = "R1",
                      hyps =
                        [ GoalHyp . mkGoal $ P1,
                          GoalHyp . mkGoal $ P2,
                          GoalHyp . mkGoal $ P3
                        ],
                      conc = Q
                    }
                ]
                  <&> augmentDerivation_Rule
                    AugmentDerivation.Config {isDerivation = \a -> Just ("?" <> coerce a)},
          goldenVsString
            "3_hypotheses_2_derivations"
            (goldenDirpath </> "rules" </> "3_hypotheses_2_derivations" <.> "golden")
            do
              return . fromString . prettyShow $
                [ Rule
                    { name = "R1",
                      hyps =
                        [ GoalHyp . mkGoal $ B,
                          GoalHyp . mkGoal $ P1,
                          GoalHyp . mkGoal $ P2
                        ],
                      conc = Q
                    }
                ]
                  <&> augmentDerivation_Rule
                    AugmentDerivation.Config
                      { isDerivation = \case
                          a | a `elem` ["P1", "P2"] -> Just ("?" <> coerce a)
                          _ | otherwise -> Nothing
                      }
        ],
      let mkTestGroup groupName cfg_engine_pre cfg_augmentDerivation =
            testGroup
              groupName
              [ goldenVsString (groupName <> "_pre_rules") (goldenDirpath </> "solutions" </> (groupName <> "_pre_rules") <.> "golden") do
                  return . fromString . render $
                    vcat
                      [ hang "rules =" 2 (pPrint cfg_engine_pre.rules),
                        hang "goals =" 2 (pPrint cfg_engine_pre.goals)
                      ],
                mkTest_Engine (groupName <> "_pre") cfg_engine_pre EngineSuccess,
                goldenVsString (groupName <> "_post_rules") (goldenDirpath </> "solutions" </> (groupName <> "_post_rules") <.> "golden") do
                  return . fromString . render $
                    vcat
                      [ hang "rules =" 2 (pPrint cfg_engine_post.rules),
                        hang "goals =" 2 (pPrint cfg_engine_post.goals)
                      ],
                mkTest_Engine (groupName <> "_post") cfg_engine_post EngineSuccess
              ]
            where
              cfg_engine_post = augmentDerivation cfg_augmentDerivation cfg_engine_pre
       in testGroup
            "solutions"
            [ mkTestGroup
                "ex1"
                Engine.Config
                  { initialGas = FiniteGas 50,
                    rules =
                      [ Rule
                          { name = "T_Z",
                            hyps = [],
                            conc = T Z
                          },
                        Rule
                          { name = "T_Sn",
                            hyps =
                              [ GoalHyp . mkGoal $ T "n",
                                GoalHyp . mkGoal $ T "n"
                              ],
                            conc = T (S "n")
                          }
                      ],
                    goals = [mkGoal $ T (S (S (S (S Z))))],
                    exprAliases = [],
                    shouldSuspend = const False,
                    strategy = DepthFirstStrategy
                  }
                AugmentDerivation.Config
                  { isDerivation = \a -> Just ("?" <> coerce a)
                  }
            ]
    ]

pattern P1 :: Atom A C V
pattern P1 = Atom "P1" []

pattern P2 :: Atom A C v
pattern P2 = Atom "P2" []

pattern P3 :: Atom A C v
pattern P3 = Atom "P3" []

pattern B :: Atom A C v
pattern B = Atom "B" []

pattern Q :: Atom A C v
pattern Q = Atom "Q" []

pattern Z :: Expr C V
pattern Z = ConExpr (Con "Z" [])

pattern S :: Expr C V -> Expr C V
pattern S n = ConExpr (Con "S" [n])

pattern T :: Expr C V -> Atom A C V
pattern T n = Atom "T" [n]
