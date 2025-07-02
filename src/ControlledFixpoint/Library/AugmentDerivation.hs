-- TODO
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ControlledFixpoint.Library.AugmentDerivation where

import Control.Lens (FoldableWithIndex (ifoldMap), FunctorWithIndex (imap))
import ControlledFixpoint.Grammar
import Data.Function
import Data.Functor

data Config = Config
  { isSubDerivation :: Atom -> Bool,
    getDerivationVarOfAtom :: Atom -> Int -> Maybe Var
  }

augmentDerivation :: Config -> Rule -> Rule
augmentDerivation cfg rule =
  let drvs =
        rule.hyps & ifoldMap \i -> \case
          AtomHyp (Atom c es) -> case cfg.getDerivationVarOfAtom (Atom c es) i of
            Nothing -> []
            Just drv -> [VarExpr drv]
   in Rule
        { name = rule.name,
          hyps =
            rule.hyps & imap \i -> \case
              AtomHyp (Atom c es) -> case cfg.getDerivationVarOfAtom (Atom c es) i of
                Nothing -> AtomHyp (Atom c es)
                Just drv -> AtomHyp (Atom c (es <> [VarExpr drv])),
          conc = case rule.conc of
            Atom c es -> Atom c (es <> drvs)
        }
