-- TODO
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ControlledFixpoint.Library.AugmentDerivation where

import ControlledFixpoint.Grammar
import Data.Functor

data Config = Config
  {}

-- xxx ::

augmentDerivation :: Config -> Rule -> Rule
augmentDerivation cfg rule =
  Rule
    { name = rule.name,
      hyps =
        rule.hyps <&> \case
          AtomHyp a -> AtomHyp a,
      conc = rule.conc
    }
