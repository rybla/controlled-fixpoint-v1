{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- While this module does in fact demonstrate engine runs where a goal gets delayed
-- and then resumed, it doesn't yet show any examples of runs that would be
-- _impossible_ unless this delay-resume action happened.
--
-- I'm starting to wonder if it's always necessary that delaying and resuming can
-- never make new things solvable if you try rules non-deterministically. Since,
-- delaying can never make it so that in the future a _new_ rule would apply to
-- that goal -- all that can happen to a goal while it's delayed is for it to be
-- refined. So, the other non-deterministic option would be to just apply that
-- alternative rule right away.
--
-- So my current hypothesis is that non-deterministically applying rules allows you
-- to avoid the traps that delaying was originally invented to avoid, but delaying
-- is still useful as an optimization to preemptively prune those branches we know
-- are going to turn out badly.
module Spec.Engine.DelayAndResume (tests) where

import ControlledFixpoint.Engine (Config (initialGas))
import qualified ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
import Spec.Engine.Common
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "DelayAndResume"
    [ mkTest_Engine
        "Ex1"
        ( Engine.Config
            { initialGas = 100,
              rules =
                [ Rule
                    { name = "x ~ x",
                      hyps = [],
                      conc = Rel "x" "x"
                    },
                  Rule
                    { name = "A ~ B",
                      hyps = [],
                      conc = Rel A B
                    }
                ],
              delayable = \case
                Rel (VarExpr _) (VarExpr _) -> True
                _ -> False,
              goals = [Rel "y" "x", Rel A "y", Rel "x" B]
            }
        )
        (EngineSuccess Nothing)
    ]

pattern Rel :: Expr -> Expr -> Atom
pattern Rel x y = Atom "atom" (ConExpr (Con "rel" [x, y]))

pattern A :: Expr
pattern A = ConExpr (Con "A" [])

pattern B :: Expr
pattern B = ConExpr (Con "B" [])
