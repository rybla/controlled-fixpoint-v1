{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Utility where

import Data.Kind (Type)
import Data.Traversable (for)

infixl 1 &

(&) :: a -> (a -> b) -> b
(&) = flip ($)

(&!) :: a -> (a -> b) -> b
(&!) = flip ($!)

infixl 1 &!

(<&>) :: (Functor f) => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

infixl 1 <&>

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

(=<<$>) :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
(=<<$>) = traverse

infixl 4 =<<$>

(<&>>=) :: (Applicative f, Traversable t) => t a -> (a -> f b) -> f (t b)
(<&>>=) = for

infixr 4 <&>>=

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f = go []
  where
    go acc [] = reverse acc
    go acc (x : xs) = case f x of
      Nothing -> go acc xs
      Just y -> go (y : acc) xs
