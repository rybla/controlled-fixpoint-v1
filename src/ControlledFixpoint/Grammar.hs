{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ControlledFixpoint.Grammar where

import Text.PrettyPrint.HughesPJ (hcat, hsep, parens, punctuate, text, (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Utility

-- | Relation
data Rel = Rel RelName [Expr]
  deriving (Show, Eq, Ord)

instance Pretty Rel where
  pPrint (Rel r es) = pPrint r <+> parens (es <&> pPrint & hsep)

--------------------------------------------------------------------------------
-- Expr
--------------------------------------------------------------------------------

-- | Expression
data Expr
  = VarExpr Var
  | ConExpr Con
  deriving (Show, Eq, Ord)

instance Pretty Expr where
  pPrint = undefined

-- | Meta-variable that can be substituted with an expression
newtype Var = Var String
  deriving (Show, Eq, Ord)

instance Pretty Var where
  pPrint (Var x) = text x

-- | Constructor expression
data Con = Con ConName [Expr]
  deriving (Show, Eq, Ord)

instance Pretty Con where
  pPrint (Con c es) = pPrint c <+> parens (es <&> pPrint & punctuate " " & hcat)

--------------------------------------------------------------------------------
-- Names
--------------------------------------------------------------------------------

-- | Relation name
newtype RelName = RelName String
  deriving (Show, Eq, Ord)

instance Pretty RelName where
  pPrint (RelName x) = text x

-- | Constructor name
newtype ConName = ConName String
  deriving (Show, Eq, Ord)

instance Pretty ConName where
  pPrint (ConName x) = text x
