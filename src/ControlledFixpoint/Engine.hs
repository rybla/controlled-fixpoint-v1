{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ControlledFixpoint.Engine where

import Control.Monad.Identity (Identity)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import ControlledFixpoint.Grammar
import Data.Map (Map)
import qualified Data.Map as Map
import ListT (ListT (..))
import Text.PrettyPrint.HughesPJ (braces, comma, hsep, nest, punctuate, text, ($+$))
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Utility

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

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

data Context = Context
  {
  }
  deriving (Show)

data State = State
  { gas :: Int,
    sigma :: Subst,
    delayedGoals :: [Rel],
    activeGoals :: [Rel]
  }
  deriving (Show)

data Log = Log String String
  deriving (Show)

instance Pretty Log where
  pPrint (Log lbl msg) = text lbl $+$ nest 1 (text msg)

type M =
  (ReaderT Context)
    ( (WriterT [Log])
        ( ListT
            ( (StateT State)
                Identity
            )
        )
    )

--------------------------------------------------------------------------------
-- Running
--------------------------------------------------------------------------------

run :: M ()
run = undefined

step :: M ()
step = undefined
