{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use camelCase" #-}

module ControlledFixpoint.Grammar where

import Control.Monad (unless, when)
import Control.Monad.Error.Class (MonadError (throwError))
import qualified ControlledFixpoint.Common as Common
import ControlledFixpoint.Common.Msg (Msg (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString (fromString))
import Text.PrettyPrint.HughesPJ (braces, comma, hcat, hsep, nest, parens, punctuate, text, vcat, (<+>))
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
    vcat
      [ "rule" <+> pPrint rule.name,
        nest 2 . vcat $
          [ vcat (rule.hyps <&> pPrint),
            "--------------------------------",
            pPrint rule.conc
          ]
      ]

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
  pPrint rel = pPrint rel.name <+> (rel.arg & pPrint)

--------------------------------------------------------------------------------
-- Expr
--------------------------------------------------------------------------------

-- | Expression
data Expr
  = VarExpr Var
  | ConExpr Con
  deriving (Show, Eq, Ord)

instance Pretty Expr where
  pPrint (VarExpr x) = pPrint x
  pPrint (ConExpr (Con c es)) = pPrint c <+> hsep (es <&> pPrint)

-- | Meta-variable that can be substituted with an expression
data Var = Var String (Maybe Int)
  deriving (Show, Eq, Ord)

instance IsString Var where fromString s = Var s Nothing

instance Pretty Var where
  pPrint (Var x Nothing) = text x
  pPrint (Var x (Just i)) = text x <> "#" <> pPrint i

-- | Constructor expression
data Con = Con ConName [Expr]
  deriving (Show, Eq, Ord)

instance Pretty Con where
  pPrint (Con c []) = pPrint c
  pPrint (Con c es) = pPrint c <+> parens (es <&> pPrint & punctuate " " & hcat)

-- | Substitution of meta-variables
newtype Subst = Subst {unSubst :: Map Var Expr}
  deriving (Show)

instance Pretty Subst where
  pPrint (Subst m) =
    braces $
      m
        & Map.toList
        <&> (\(x, e) -> pPrint x <+> ":=" <+> pPrint e)
        & punctuate comma
        & hsep

--------------------------------------------------------------------------------
-- Names
--------------------------------------------------------------------------------

-- | Rule name
newtype RuleName = RuleName String
  deriving (Show, Eq, Ord)

instance IsString RuleName where fromString = RuleName

instance Pretty RuleName where pPrint (RuleName x) = text x

-- | Relation name
newtype RelName = RelName String
  deriving (Show, Eq, Ord)

instance IsString RelName where fromString = RelName

instance Pretty RelName where pPrint (RelName x) = text x

-- | Constructor name
newtype ConName = ConName String
  deriving (Show, Eq, Ord)

instance IsString ConName where fromString = ConName

instance Pretty ConName where pPrint (ConName x) = text x

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

varsRel :: Rel -> Set Var
varsRel (Rel _ e) = e & varsExpr

varsExpr :: Expr -> Set Var
varsExpr (VarExpr x) = Set.singleton x
varsExpr (ConExpr (Con _ es)) = es <&> varsExpr & Set.unions

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

-- | Throws an error if 'sigma'' substitutes a variable that is also substituted
-- by 'sigma'.
composeSubst :: (Monad m) => Subst -> Subst -> Common.T m Subst
composeSubst sigma@(Subst m) _sigma'@(Subst m') = do
  let keysIntersection = (m & Map.keysSet) `Set.intersection` (m' & Map.keysSet)
  unless (Set.null keysIntersection) do
    throwError $
      Msg
        "In 'substSubst', there are variables that both substitutions substitute"
        ["keysIntersection =" <+> (keysIntersection & Set.toList & pPrint)]
  return $ Subst $ m `Map.union` (m' <&> substExpr sigma)
