{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Engine.PruneAtRequiredGoalFailure (tests) where

import qualified ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
import qualified Data.Set as Set
import Spec.Engine.Common (mkTest_Engine)
import qualified Spec.Engine.Common as Common
import Test.Tasty (TestTree, testGroup)
import Text.PrettyPrint.HughesPJClass (Pretty (..), text)

data A = P | Q | R deriving (Show, Eq, Ord)

instance Pretty A where pPrint = text . show

data C = A | B deriving (Show, Eq, Ord)

instance Pretty C where pPrint = text . show

type V = String

tests :: TestTree
tests =
  testGroup
    "PruneAtRequiredGoalFailure"
    [ -- Due to the `RequiredGoalOpt` on the first hypothesis of `Rule1`, it will not infinitely loop because the first use of `Rule1` will fail to solve that hypothesis which will prune that branch. And so the result will be an `EngineFailure` (`failedGoals` is populated) rather that an `EngineError` (due to running out of gas)
      mkTest_Engine @A @C @V
        "ex1"
        Engine.Config
          { initialGas = Engine.FiniteGas 5,
            rules =
              [ (mkRule "Rule1")
                  [ GoalHyp (mkHypGoal (Atom Q [])) {goalOpts = Set.fromList [RequiredGoalOpt]},
                    GoalHyp (mkHypGoal (Atom P []))
                  ]
                  (Atom P [])
              ],
            goals =
              [ mkGoal 0 $ Atom P []
              ],
            shouldSuspend = const False,
            exprAliases = [],
            strategy = Engine.DepthFirstStrategy
          }
        Common.EngineFailure
    ]
