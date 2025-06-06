{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use camelCase" #-}

module ControlledFixpoint.Grammar where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.PrettyPrint.HughesPJ (braces, comma, hcat, hsep, nest, parens, punctuate, text, vcat, ($+$), (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Utility

--------------------------------------------------------------------------------
-- Rule
--------------------------------------------------------------------------------

-- | Rule
data Rule = Rule
  { name :: RuleName,
    hyps :: [Hyp],
    conc :: Rel
  }
  deriving (Show, Eq, Ord)

instance Pretty Rule where
  pPrint rule =
    "rule" <+> pPrint rule.name
      $+$ nest 2 (vcat (rule.hyps <&> pPrint))
      $+$ "----------------"
      $+$ pPrint rule.conc

data Hyp = RelHyp Rel
  deriving (Show, Eq, Ord)

instance Pretty Hyp where
  pPrint (RelHyp rel) = pPrint rel

--------------------------------------------------------------------------------
-- Relation
--------------------------------------------------------------------------------

-- | Relation
data Rel = Rel
  { name :: RelName,
    arg :: Expr
  }
  deriving (Show, Eq, Ord)

instance Pretty Rel where
  pPrint rel = pPrint rel.name <+> parens (rel.arg & pPrint)

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

-- | Substitution of meta-variables
newtype Subst = Subst {unSubst :: Map Var Expr}
  deriving (Show)

instance Pretty Subst where
  pPrint (Subst m) =
    braces $
      m
        & Map.toList
        <&> (\(x, e) -> pPrint x <> pPrint e)
        & punctuate comma
        & hsep

--------------------------------------------------------------------------------
-- Names
--------------------------------------------------------------------------------

-- | Rule name
newtype RuleName = RuleName String
  deriving (Show, Eq, Ord)

instance Pretty RuleName where
  pPrint (RuleName x) = text x

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

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

varsRel :: Rel -> Set Var
varsRel = undefined

varsExpr :: Expr -> Set Var
varsExpr = undefined

occursInRel :: Var -> Rel -> Bool
occursInRel x r = x `Set.member` varsRel r

occursInExpr :: Var -> Expr -> Bool
occursInExpr x e = x `Set.member` varsExpr e

emptySubst :: Subst
emptySubst = Subst Map.empty

setVar :: Var -> Expr -> Subst -> Subst
setVar x e (Subst m) = Subst (Map.insert x e m)

substHyp :: Subst -> Hyp -> Hyp
substHyp sigma (RelHyp rel) = RelHyp (substRel sigma rel)

substRel :: Subst -> Rel -> Rel
substRel sigma r =
  Rel
    { name = r.name,
      arg = r.arg & substExpr sigma
    }

substExpr :: Subst -> Expr -> Expr
substExpr (Subst m) (VarExpr x) = case m Map.!? x of
  Nothing -> VarExpr x
  Just e -> e
substExpr sigma (ConExpr (Con c es)) =
  ConExpr (Con c (es <&> substExpr sigma))

substVar :: Subst -> Var -> Maybe Expr
substVar (Subst m) x = m Map.!? x
