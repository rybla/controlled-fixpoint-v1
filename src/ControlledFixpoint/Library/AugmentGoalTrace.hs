{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ControlledFixpoint.Library.AugmentGoalTrace where

import Control.Lens (FunctorWithIndex (imap))
import qualified ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
import Data.Coerce (coerce)
import Data.Function
import Data.Functor ((<&>))
import qualified Data.List.Safe as List
import Data.String (IsString (..))
import Text.PrettyPrint (render, vcat, (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (..))
import Utility (bullets)

data Config = Config {}

-- | `augmentGoalTrace` is an `Engine.Config` tranformation that augments each
-- atom with a **goal trace** as an additional argument. The `Engine.goals` are
-- each assigned a unique goal trace constant. Each rule is updated to propagate
-- the goal trace of the conclusion atom the the goal traces of the hypothis
-- atoms.
augmentGoalTrace :: Config -> Engine.Config -> Engine.Config
augmentGoalTrace cfg cfg_engine =
  cfg_engine
    { Engine.rules = cfg_engine.rules & fmap (augmentGoalTrace_Rule cfg),
      Engine.goals = cfg_engine.goals & imap (augmentGoalTrace_Goal cfg),
      Engine.shouldSuspend = \case
        Atom a es -> case List.init es of
          Nothing ->
            error . render . vcat $
              [ "BUG: in the `shouldSuspend` function created by `augmentGoalTrace`, an atom does not have at least one argument, which should have been inserted already since the last argument of an atom encodes the goal trace",
                bullets ["atom =" <+> pPrint a]
              ]
          Just es' -> cfg_engine.shouldSuspend (Atom a es')
    }

augmentGoalTrace_Goal :: Config -> Int -> Atom -> Atom
augmentGoalTrace_Goal _cfg i (Atom a es) = Atom a (es <> [con (coerce ("goal#" <> show i)) []])

augmentGoalTrace_Rule :: Config -> Rule -> Rule
augmentGoalTrace_Rule _cfg rule =
  rule
    { hyps =
        rule.hyps <&> \case
          AtomHyp (Atom a es) -> AtomHyp (Atom a (es <> [goal])),
      conc = case rule.conc of
        Atom a es -> Atom a (es <> [goal])
    }
  where
    goal = var (fromString "?goal")
