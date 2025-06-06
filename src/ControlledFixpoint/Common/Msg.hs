{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ControlledFixpoint.Common.Msg where

import Text.PrettyPrint.HughesPJ (Doc, nest, vcat, ($+$), (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))

data Msg = Msg {title :: Doc, contents :: [Doc]}
  deriving (Show)

instance Pretty Msg where
  pPrint m =
    m.title
      $+$ "•" <+> nest 2 (vcat m.contents)

addContent :: Doc -> Msg -> Msg
addContent content msg = msg {contents = msg.contents <> [content]}
