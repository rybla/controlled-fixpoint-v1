{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ControlledFixpoint.Main (main) where

import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Control.Monad.Writer (WriterT (runWriterT))
import qualified ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
import Data.String (IsString (fromString))
import Text.PrettyPrint (hang, render)
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Utility

(==.) :: Expr -> Expr -> Expr
x ==. y = ConExpr (Con "equal" [x, y])

infix 4 ==.

(+.) :: Expr -> Expr -> Expr
x +. y = ConExpr (Con "add" [x, y])

infixl 6 +.

varExpr :: String -> Expr
varExpr x = VarExpr (Var x Nothing)

sucExpr :: Expr -> Expr
sucExpr x = ConExpr (Con "suc" [x])

zeroExpr :: Expr
zeroExpr = ConExpr (Con "zero" [])

instance IsString Expr where fromString = varExpr

instance Num Expr where
  (+) = (+.)
  (*) = undefined
  fromInteger n | n < 0 = undefined
  fromInteger 0 = zeroExpr
  fromInteger n = sucExpr (fromInteger (n - 1))

  abs = undefined
  signum = undefined
  negate = undefined

cfg_0 :: Engine.Config
cfg_0 =
  Engine.Config
    { initialGas = 100,
      rules =
        [ Rule
            { name = RuleName $ show @String "0 + x = x",
              hyps = [],
              conc = Atom "IsTrue" (0 + "x" ==. "x")
            },
          Rule
            { name = RuleName $ show @String "x + y = z ==> suc x + y = suc z",
              hyps =
                [AtomHyp $ Atom "IsTrue" ("x" +. "y" ==. "z")],
              conc =
                Atom "IsTrue" (sucExpr "x" +. "y" ==. sucExpr "z")
            } {-,
              Rule
                { name = RuleName $ show @String "x + 0 = x",
                  hyps = [],
                  conc = Atom "IsTrue" ("x" + 0 ==. "x")
                },
              Rule
                { name = RuleName $ show @String "x + y = z ==> x + suc y = suc z",
                  hyps =
                    [AtomHyp $ Atom "IsTrue" ("x" +. "y" ==. "z")],
                  conc =
                    Atom "IsTrue" ("x" +. sucExpr "y" ==. sucExpr "z")
                }-}
        ],
      goals =
        [ Atom "IsTrue" ((1 +. 2) ==. 2)
        ]
    }

main :: IO ()
main = do
  (err_or_envs, msgs) <-
    Engine.run cfg_0
      & runExceptT
      & runWriterT
  putStrLn "================================"
  putStrLn " logs "
  putStrLn "================================"
  void $ msgs <&>>= \msg -> putStrLn $ render $ pPrint msg
  putStrLn "================================"
  putStrLn " result "
  putStrLn "================================"
  case err_or_envs of
    Left err -> putStrLn $ render (hang "err:" 2 $ pPrint err)
    Right envs -> putStrLn $ render (hang "envs:" 2 $ bullets (envs <&> pPrint))
  return ()
