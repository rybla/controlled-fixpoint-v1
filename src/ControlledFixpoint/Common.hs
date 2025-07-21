{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ControlledFixpoint.Common where

import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT)
import ControlledFixpoint.Common.Msg (Msg)

type T m =
  (ExceptT Msg)
    ( (WriterT [Msg])
        m
    )

liftT :: (Monad m) => m a -> T m a
liftT = lift . lift
