{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module ControlledFixpoint.Html where

import ControlledFixpoint.Engine
import ControlledFixpoint.Grammar
import Text.PrettyPrint (Doc, hang, nest, quotes, text, vcat, ($+$), (<+>), (<>))
import Prelude hiding (div, (<>))

tag :: String -> String -> [Doc] -> Doc
tag tagName className kids =
  vcat
    [ "<" <> text tagName <+> "class=" <> quotes (text className) <> ">",
      nest 4 $ vcat kids,
      "<" <> text tagName <> ">"
    ]

div :: String -> [Doc] -> Doc
div = tag "div"

-- renderConfig :: Config a c v -> Doc
-- renderConfig = _
