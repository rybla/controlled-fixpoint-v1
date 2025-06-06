{-# LANGUAGE OverloadedStrings #-}

module ControlledFixpoint.Main (main) where

import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Control.Monad.Writer (WriterT (runWriterT))
import qualified ControlledFixpoint.Engine as Engine
import ControlledFixpoint.Grammar
import Text.PrettyPrint (hang, render)
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import Utility

equalExpr :: Expr -> Expr -> Expr
equalExpr x y = ConExpr (Con "equal" [x, y])

addExpr :: Expr -> Expr -> Expr
addExpr x y = ConExpr (Con "add" [x, y])

varExpr :: String -> Expr
varExpr x = VarExpr (Var x Nothing)

sucExpr :: Expr -> Expr
sucExpr x = ConExpr (Con "suc" [x])

zeroExpr :: Expr
zeroExpr = ConExpr (Con "zero" [])

main :: IO ()
main = do
  let cfg =
        Engine.Config
          { initialGas = 100,
            rules =
              [ Rule
                  { name = "\"0 + x = x\"",
                    hyps = [],
                    conc = Rel "IsTrue" (equalExpr (addExpr zeroExpr (varExpr "x")) (varExpr "x"))
                  },
                Rule
                  { name = "\"x + y = z ==> suc x + y = suc z\"",
                    hyps =
                      [RelHyp $ Rel "IsTrue" (equalExpr (addExpr (varExpr "x") (varExpr "y")) (varExpr "z"))],
                    conc =
                      Rel "IsTrue" (equalExpr (addExpr (sucExpr (varExpr "x")) (varExpr "y")) (sucExpr (varExpr "z")))
                  }
              ],
            goals =
              [ Rel "IsTrue" (equalExpr (addExpr (sucExpr zeroExpr) zeroExpr) (sucExpr zeroExpr))
              ]
          }
  (err_or_envs, msgs) <-
    Engine.run cfg
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
