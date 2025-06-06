{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ControlledFixpoint.Common.Msg where

import Text.PrettyPrint.HughesPJ (Doc, hang)
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Utility

data Msg = Msg {title :: Doc, contents :: [Doc]}
  deriving (Show)

instance Pretty Msg where
  pPrint m =
    hang m.title 2 $
      bullets m.contents

addContent :: Doc -> Msg -> Msg
addContent content msg = msg {contents = msg.contents <> [content]}
