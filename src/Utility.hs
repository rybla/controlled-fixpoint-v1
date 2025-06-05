{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Utility where

infixl 1 &

infixl 1 &!

(&) :: a -> (a -> b) -> b
(&) = flip ($)

(&!) :: a -> (a -> b) -> b
(&!) = flip ($!)

infixl 1 <&>

(<&>) :: (Functor f) => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
