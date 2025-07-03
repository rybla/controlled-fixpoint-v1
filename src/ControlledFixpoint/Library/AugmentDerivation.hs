{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ControlledFixpoint.Library.AugmentDerivation where

import Control.Lens (FunctorWithIndex (imap))
import qualified ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
import Data.Bifunctor (second)
import Data.Coerce (coerce)
import Data.Function
import Data.Functor ((<&>))
import qualified Data.List.Safe as List
import Data.String (IsString (..))
import Text.PrettyPrint (render, vcat, (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (..))
import Utility (bullets)

data Config = Config
  { isDerivation :: AtomName -> Maybe String
  }

-- | `augmentDerivation` is an `Engine.Config` tranformation that promotes some
-- of its atoms as **derivations** (as defined in the `Config`). A derivation
-- atom gets an additional argument which corresponds to its **derivation
-- tree**. The derivation tree has a node for each time a rule was used, and the
-- children of that node are the derivation trees for each of the hypotheses of
-- that rule use.
--
-- The variable names of the new arguments are specified by the `isDerivation`
-- function, and also given a number corresponding to the index of the
-- hypothesis that the atom is used in.
--
-- The nodes of the derivation tree use new expression constructors that are
-- derived from the names of the rules they correspond to.
--
-- See the goldenfile tests for this module for examples of what this
-- augmentation looks like.
augmentDerivation :: Config -> Engine.Config -> Engine.Config
augmentDerivation cfg cfg_engine =
  cfg_engine
    { Engine.rules = cfg_engine.rules <&> augmentDerivation_Rule cfg,
      Engine.goals = cfg_engine.goals & imap (augmentDerivation_Atom cfg),
      Engine.shouldSuspend = \case
        a@(Atom n es) | Just _ <- cfg.isDerivation n -> case List.init es of
          Nothing ->
            error . render . vcat $
              [ "BUG: in the `shouldSuspend` function created by `augmentDerivation`, a derivation atom does not have at least one argument, which should have been inserted already since the last argument of a derivation atom encodes the derivation itself",
                bullets ["atom =" <+> pPrint a]
              ]
          Just es' -> cfg_engine.shouldSuspend (Atom n es')
        a -> cfg_engine.shouldSuspend a
    }

augmentDerivation_Atom :: Config -> Int -> Atom -> Atom
augmentDerivation_Atom cfg i (Atom a es)
  | Just s <- cfg.isDerivation a,
    x <- var (fromString (s <> show i)) =
      Atom a (es <> [x])
augmentDerivation_Atom _ _ a = a

augmentDerivation_Rule :: Config -> Rule -> Rule
augmentDerivation_Rule cfg rule =
  let (hyps', hypDrvs) =
        rule.hyps
          & imap
            ( \i -> \case
                AtomHyp (Atom c es)
                  | Just s <- cfg.isDerivation c,
                    x <- var (fromString (s <> show i)) ->
                      (AtomHyp $ Atom c (es <> [x]), [x])
                h -> (h, [])
            )
          & unzip
          & second concat
   in rule
        { hyps = hyps',
          conc = case rule.conc of
            Atom c es | Just _ <- cfg.isDerivation c -> Atom c (es <> [con (coerce rule.name) hypDrvs])
            a -> a
        }
