{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ControlledFixpoint.Grammar where

import Control.Monad (foldM, zipWithM)
import Control.Monad.Except (Except, MonadError (throwError))
import ControlledFixpoint.Common.Msg (Msg (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.PrettyPrint.HughesPJ (braces, comma, hcat, hsep, parens, punctuate, text, (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Utility

--------------------------------------------------------------------------------
-- Rule
--------------------------------------------------------------------------------

-- | Rule
data Rule = Rule RuleName [Hyp] Rel
  deriving (Show, Eq, Ord)

data Hyp = RelHyp Rel
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Relation
--------------------------------------------------------------------------------

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

-- | Substitution of meta-variables
newtype Subst = Subst (Map Var Expr)
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

emptySubst :: Subst
emptySubst = Subst Map.empty

substRel :: Subst -> Rel -> Rel
substRel (Subst m) r = undefined

substExpr :: Subst -> Expr -> Expr
substExpr (Subst m) (VarExpr x) = case m Map.!? x of
  Nothing -> VarExpr x
  Just e -> e
substExpr sigma (ConExpr (Con c es)) =
  ConExpr (Con c (es <&> substExpr sigma))

-- | Merges a collection of 'Subst's into a single 'Subst', if they are
-- compatible. If a pair of 'Subst's substitute the same variable, then the
-- substituting expressions are unified.
mergeSubsts :: [Subst] -> Except Msg Subst
mergeSubsts = undefined

unifyExpr :: Expr -> Expr -> Except Msg Subst
unifyExpr (VarExpr x1) e2
  | Set.member x1 (varsExpr e2) =
      throwError
        ( Msg
            { title = "You cannot unify a variable expression with an expression that refers to that variable.",
              contents =
                [ "variable   :" <+> pPrint x1,
                  "expression :" <+> pPrint e2
                ]
            }
        )
  | otherwise = return (Subst (Map.singleton x1 e2))
unifyExpr e1 (VarExpr x2)
  | Set.member x2 (varsExpr e1) =
      throwError
        ( Msg
            { title = "You cannot unify a variable expression with an expression that refers to that variable.",
              contents =
                [ "variable   :" <+> pPrint x2,
                  "expression :" <+> pPrint e1
                ]
            }
        )
  | otherwise = return (Subst (Map.singleton x2 e1))
unifyExpr e1@(ConExpr (Con c1 es1)) e2@(ConExpr (Con c2 es2))
  | c1 /= c2 =
      throwError
        ( Msg
            { title = "You cannot unify constructor expressions with different constructors.",
              contents =
                [ "expression #1 :" <+> pPrint e1,
                  "expression #2 :" <+> pPrint e2
                ]
            }
        )
  | otherwise = zipWithM unifyExpr es1 es2 >>= mergeSubsts
