{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Engine.Library.AugmentGoalTrace (tests) where

import ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
import ControlledFixpoint.Library.AugmentGoalTrace as AugmentGoalTrace
import Data.String (IsString (fromString))
import Spec.Engine.Common (EngineResult (EngineSuccess), mkTest_Engine)
import qualified Spec.Engine.Library.Common as Common
import System.FilePath ((<.>), (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Text.PrettyPrint (hang, render, vcat)
import Text.PrettyPrint.HughesPJClass (Pretty (..))

goldenDirpath :: FilePath
goldenDirpath = Common.goldenDirpath </> "AugmentGoalTrace"

tests :: TestTree
tests =
  testGroup
    "AugmentGoalTrace"
    let mkTestGroup groupName cfg_engine_pre cfg_augmentGoalTrace =
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
            cfg_engine_post = augmentGoalTrace cfg_augmentGoalTrace cfg_engine_pre
     in [ mkTestGroup
            "ex1"
            Engine.Config
              { initialGas = FiniteGas 50,
                rules =
                  [ Rule {name = "R1", hyps = [AtomHyp B], conc = A},
                    Rule {name = "R2", hyps = [], conc = B}
                  ],
                goals =
                  [A],
                shouldSuspend = const False,
                strategy = DepthFirstStrategy
              }
            AugmentGoalTrace.Config {},
          mkTestGroup
            "ex2"
            Engine.Config
              { initialGas = FiniteGas 50,
                rules =
                  [ Rule {name = "R1", hyps = [AtomHyp B], conc = A},
                    Rule {name = "R2", hyps = [], conc = B}
                  ],
                goals =
                  [A, A],
                shouldSuspend = const False,
                strategy = DepthFirstStrategy
              }
            AugmentGoalTrace.Config {}
        ]

pattern A :: Atom
pattern A = Atom "A" []

pattern B :: Atom
pattern B = Atom "B" []
