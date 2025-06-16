{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Utility where

import Control.Monad (foldM)
import Data.Kind (Type)
import Data.Traversable (for)
import Text.PrettyPrint (Doc, nest, render, vcat, (<+>))
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))

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
-- filterMap f = go []
--   where
--     go acc [] = reverse acc
--     go acc (x : xs) = case f x of
--       Nothing -> go acc xs
--       Just y -> go (y : acc) xs
filterMap f = foldMap \x -> case f x of
  Nothing -> []
  Just y -> [y]

foldMapM :: (Foldable t, Monad m, Monoid b) => (a -> m b) -> t a -> m b
foldMapM f = foldM (\m a -> (m <>) <$> f a) mempty

bullets :: [Doc] -> Doc
bullets = vcat . fmap (("•" <+>) . nest 2)

extractAtIndex :: Int -> [a] -> Maybe ([a], a)
extractAtIndex = go []
  where
    go :: [a] -> Int -> [a] -> Maybe ([a], a)
    go ys 0 (x : xs) = return (reverse ys <> xs, x)
    go ys i (x : xs) = go (x : ys) (i - 1) xs
    go _ _ [] = Nothing

extractions :: [a] -> [([a], a)]
extractions = go [] []
  where
    go :: [([a], a)] -> [a] -> [a] -> [([a], a)]
    go outputs _ [] = outputs
    go outputs xs (y : ys) = go (outputs <> [(xs <> ys, y)]) (xs <> [y]) ys

indices :: [a] -> [Int]
indices xs = [0 .. length xs - 1]
