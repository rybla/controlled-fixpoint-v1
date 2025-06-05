{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Utility where

import Data.Kind (Type)

infixl 1 &

infixl 1 &!

(&) :: a -> (a -> b) -> b
(&) = flip ($)

(&!) :: a -> (a -> b) -> b
(&!) = flip ($!)

infixl 1 <&>

(<&>) :: (Functor f) => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

-- | Infix application.
--
-- @
-- f :: Either String $ Maybe Int
-- =
-- f :: Either String (Maybe Int)
-- @
--
-- Inspired by: https://hackage.haskell.org/package/type-operators
type (f :: Type -> Type) $ (a :: Type) = f a

infixr 2 $
