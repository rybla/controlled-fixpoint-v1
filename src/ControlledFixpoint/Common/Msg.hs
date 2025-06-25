{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ControlledFixpoint.Common.Msg where

import Text.PrettyPrint (Doc, nest, ($+$))
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Utility

data Msg = Msg
  { title :: Doc,
    contents :: [Doc],
    level :: Level
  }
  deriving (Show, Eq)

instance Pretty Msg where
  pPrint m =
    m.title
      $+$ nest 2 (bullets m.contents)

mk :: Doc -> Msg
mk title =
  Msg
    { title,
      contents = mempty,
      level = Level 0
    }

addContent :: Doc -> Msg -> Msg
addContent content msg = msg {contents = msg.contents <> [content]}

newtype Level = Level Int
  deriving (Show, Eq)

instance Pretty Level where pPrint (Level l) = pPrint l
