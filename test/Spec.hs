{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Main (main) where

import qualified Spec.Engine.Add as Test.Engine.Add
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [Test.Engine.Add.tests]

-- xs :: [Int]
-- xs = [1 .. 10]

-- test :: (Monad m) => ExceptT String (StateT [String] (ListT m)) ()
-- test = do
--   modify (<> ["start"])
--   x <- lift . lift $ foldr ListT.cons mempty xs
--   when (x == 2) do lift . lift $ mempty
--   modify (<> ["x == " <> show x])
--   when (x == 5) do throwError "error: x == 5"

-- main :: IO ()
-- main = do
--   results <-
--     test
--       & runExceptT
--       & flip runStateT []
--       & ListT.toList
--   -- putStrLn $ "logs:\n" <> (logs & foldMap \log -> "  • " <> log <> "\n")
--   -- putStrLn $ "y: " <> show results
--   void $
--     results <&>>= \(err_or_res, logs) -> do
--       putStrLn "run:"
--       putStrLn $ "  err_or_res: " <> show err_or_res
--       putStrLn $ "  logs: " <> intercalate ", " logs
