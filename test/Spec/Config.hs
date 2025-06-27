{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Spec.Config where

data Verbosity
  = SilentVerbosity
  | NormalVerbosity
  | LoggingVerbosity
  deriving (Show, Eq, Ord, Enum)

verbosity :: Verbosity
-- verbosity = LoggingVerbosity

verbosity = NormalVerbosity
