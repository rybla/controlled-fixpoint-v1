{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ControlledFixpoint.Grammar where

import Control.Category ((>>>))
import Control.Monad (unless)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Newtype.Generics (Newtype, over)
import ControlledFixpoint.Common.Msg (Msg)
import qualified ControlledFixpoint.Common.Msg as Msg
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.List.Safe as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString (fromString))
import GHC.Generics (Generic)
import Text.PrettyPrint (braces, comma, hcat, hsep, nest, parens, punctuate, text, vcat, (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Utility

--------------------------------------------------------------------------------
-- Rule
--------------------------------------------------------------------------------

-- | Rule
data Rule = Rule
  { name :: RuleName,
    hyps :: [Hyp],
    conc :: Atom
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

-- | Hypothesis.
--
-- NOTE: For now, there is only one kind of hypothesis: `AtomHyp`. However, I
-- made this a data type anyway since it may be desirable at some point to have
-- other kinds of hypotheses, such as functional predicates (predicates that are
-- checked by executing a `Bool`-valued function.)
data Hyp = AtomHyp Atom
  deriving (Show, Eq, Ord)

instance Pretty Hyp where
  pPrint (AtomHyp atom) = pPrint atom

--------------------------------------------------------------------------------
-- Atom
--------------------------------------------------------------------------------

-- | An 'Atom' is an atomic formula.
data Atom = Atom AtomName [Expr]
  deriving (Show, Eq, Ord)

instance Pretty Atom where
  pPrint (Atom c es) = pPrint c <+> (es <&> pPrint & hsep)

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
  pPrint (ConExpr c) = pPrint c

instance IsString Expr where fromString x = VarExpr (fromString x)

-- | Meta-variable that can be substituted with an expression
data Var = Var String (Maybe Int)
  deriving (Show, Eq, Ord)

var :: Var -> Expr
var = VarExpr

instance IsString Var where fromString s = Var s Nothing

instance Pretty Var where
  pPrint (Var x Nothing) = text (show x)
  pPrint (Var x (Just i)) = text (show x) <> text (i & subscriptNumber)

-- | Constructor expression
data Con = Con ConName [Expr]
  deriving (Show, Eq, Ord)

instance Pretty Con where
  pPrint (Con c []) = pPrint c
  pPrint (Con c es) = parens $ pPrint c <+> (es <&> pPrint & punctuate " " & hcat)

con :: ConName -> [Expr] -> Expr
con c es = ConExpr (Con c es)

pattern ConE :: ConName -> [Expr] -> Expr
pattern ConE c es = ConExpr (Con c es)

pattern (:%) :: ConName -> [Expr] -> Expr
pattern c :% es = ConExpr (Con c es)

infix 4 :%

instance IsString Con where
  fromString s = Con (fromString s) []

-- | Substitution of meta-variables
newtype Subst = Subst (Map Var Expr)
  deriving (Show, Eq, Generic)

unSubst :: Subst -> Map Var Expr
unSubst (Subst m) = m

instance Newtype Subst

instance Pretty Subst where
  pPrint (Subst m) =
    braces $
      m
        & Map.toList
        <&> (\(x, e) -> pPrint x <+> ":=" <+> pPrint e)
        & punctuate comma
        & hsep

--------------------------------------------------------------------------------
-- ExprAlias
--------------------------------------------------------------------------------

-- | An expression alias. These are unfolded lazily during unification, so as a
-- result the solver does it's best to not unfold definitions unless it's
-- required in order to unify two expressions. Note that you _do_ need to be
-- careful about recursively aliases, since a recursive alias could lead to
-- infinite unfolding during unification.
newtype ExprAlias = ExprAlias (Expr -> Maybe Expr)

unExprAlias :: ExprAlias -> (Expr -> Maybe Expr)
unExprAlias (ExprAlias f) = f

applyExprAliass :: [ExprAlias] -> Expr -> Maybe Expr
applyExprAliass ds e = foldMap (maybe [] pure . (unExprAlias >>> ($ e))) ds & List.head

--------------------------------------------------------------------------------
-- Names
--------------------------------------------------------------------------------

-- | Rule name
newtype RuleName = RuleName String
  deriving (Show, Eq, Ord)

unRuleName :: RuleName -> String
unRuleName (RuleName s) = s

instance IsString RuleName where fromString = RuleName

instance Pretty RuleName where pPrint (RuleName x) = text (show x)

-- | Atom name
newtype AtomName = AtomName String
  deriving (Show, Eq, Ord)

unAtomName :: AtomName -> String
unAtomName (AtomName s) = s

instance IsString AtomName where fromString = AtomName

instance Pretty AtomName where pPrint (AtomName x) = text (show x)

-- | Constructor name
newtype ConName = ConName String
  deriving (Show, Eq, Ord)

unConName :: ConName -> String
unConName (ConName s) = s

instance IsString ConName where fromString = ConName

instance Pretty ConName where pPrint (ConName x) = text (show x)

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

varsAtom :: Atom -> Set Var
varsAtom (Atom _ es) = es <&> varsExpr & Set.unions

varsExpr :: Expr -> Set Var
varsExpr (VarExpr x) = Set.singleton x
varsExpr (ConExpr (Con _ es)) = es <&> varsExpr & Set.unions

occursInAtom :: Var -> Atom -> Bool
occursInAtom x a = x `Set.member` varsAtom a

occursInExpr :: Var -> Expr -> Bool
occursInExpr x e = x `Set.member` varsExpr e

emptySubst :: Subst
emptySubst = Subst Map.empty

setVar :: Var -> Expr -> Subst -> Subst
setVar x e =
  over Subst $
    fmap (substExpr (Subst (Map.singleton x e)))
      . Map.insert x e

substHyp :: Subst -> Hyp -> Hyp
substHyp sigma (AtomHyp atom) = AtomHyp (substAtom sigma atom)

substAtom :: Subst -> Atom -> Atom
substAtom sigma (Atom c es) = Atom c (es <&> substExpr sigma)

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
composeSubst :: (MonadError Msg m) => Subst -> Subst -> m Subst
composeSubst sigma@(Subst m) sigma'@(Subst m') = do
  let keysIntersection = (m & Map.keysSet) `Set.intersection` (m' & Map.keysSet)
  unless (Set.null keysIntersection) do
    throwError $
      (Msg.mk "In 'composeSubst', there are variables that both substitutions substitute")
        { Msg.contents =
            [ "keysIntersection =" <+> (keysIntersection & Set.toList & pPrint),
              "sigma  =" <+> pPrint sigma,
              "sigma' =" <+> pPrint sigma'
            ]
        }
  return $ Subst $ m `Map.union` (m' <&> substExpr sigma)

-- | Similar to `composeSubst`, but doesn't check for overlaps.
composeSubst_unsafe :: Subst -> Subst -> Subst
composeSubst_unsafe sigma'@(Subst m') sigma@(Subst m) = Subst $ (m' <&> substExpr sigma) `Map.union` (m <&> substExpr sigma')
