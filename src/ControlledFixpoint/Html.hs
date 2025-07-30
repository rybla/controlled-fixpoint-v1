{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant $" #-}

module ControlledFixpoint.Html (renderConfig, renderEnv, renderHtml) where

import ControlledFixpoint.Engine
import ControlledFixpoint.Grammar
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Text.PrettyPrint (Doc, doubleQuotes, nest, text, vcat, (<+>), (<>))
import Text.PrettyPrint.HughesPJClass (Pretty, prettyShow)
import Prelude hiding (div, (<>))

el :: String -> String -> [Doc] -> Doc
el tagName className kids =
  vcat
    [ "<" <> text tagName <+> "class=" <> doubleQuotes (text className) <> ">",
      nest 2 $ vcat kids,
      "</" <> text tagName <> ">"
    ]

div :: String -> [Doc] -> Doc
div = el "div"

renderList :: [Doc] -> Doc
renderList = div "List" . fmap (div "ListItem" . pure)

--------------------------------------------------------------------------------

renderHtml :: Doc -> Doc
renderHtml content =
  vcat
    [ "<!DOCTYPE html>",
      "<html lang=\"en\">",
      "<head>",
      "  <link rel=\"stylesheet\" href=\"style.css\">",
      "</head>",
      "<body>",
      content,
      "</body>",
      "</html>"
    ]

renderConfig :: (Pretty a, Pretty c, Pretty v) => Config a c v -> Doc
renderConfig cfg =
  div "Config" $
    [ div "rules" . pure . renderList . fmap renderRule $ cfg.rules,
      div "goals" . pure . renderList . fmap renderGoal $ cfg.goals,
      div "strategy" . pure . pPrintEscaped $ cfg.strategy
    ]

renderEnv :: (Pretty a, Pretty c, Pretty v) => Config a c v -> Env a c v -> Doc
renderEnv cfg env =
  div "Env" $
    [ div "activeGoals" . pure . renderList . fmap renderGoal $ env.activeGoals,
      div "suspendedGoals" . pure . renderList . fmap renderGoal $ env.suspendedGoals,
      div "failedGoals" . pure . renderList . fmap renderGoal $ env.failedGoals,
      div "sigma" . pure . renderSubst $ env.sigma,
      div "steps" . pure . renderSteps cfg . reverse $ env.stepsRev,
      div "earlyTerminationReasons" . pure . renderList $ env.earlyTerminationReasons
    ]

renderGoal :: (Pretty v, Pretty c, Pretty a) => Goal a c v -> Doc
renderGoal g =
  div "Goal" $
    [ div "freshGoalIndex" . pure . pPrintEscaped . fromMaybe (-1) $ g.freshGoalIndex,
      div "atom" . pure . renderAtom $ g.atom,
      div "opts" . fmap renderGoalOpt . Set.toList $ g.opts
    ]

renderGoalOpt :: GoalOpt -> Doc
renderGoalOpt = div "GoalOpt" . pure . pPrintEscaped

renderSubst :: (Pretty c, Pretty v) => Subst c v -> Doc
renderSubst =
  div "Subst"
    . fmap
      (\(x, e) -> div "SubstItem" [renderVar x, ":=", renderExpr e])
    . Map.toList
    . unSubst

renderExpr :: (Pretty c, Pretty v) => Expr c v -> Doc
renderExpr (ConExpr (Con c es)) = div "Con" (div "ConName" [pPrintEscaped c] : fmap renderExpr es)
renderExpr (VarExpr v) = renderVar v

renderVar :: (Pretty v) => Var v -> Doc
renderVar (Var v Nothing) = div "Var" [pPrintEscaped v]
renderVar (Var v (Just i)) = div "Var" [pPrintEscaped v, div "VarFreshIndex" [pPrintEscaped i]]

renderSteps :: forall a c v. (Pretty v, Pretty c, Pretty a) => Config a c v -> [Step a c v] -> Doc
renderSteps cfg ss =
  let graph_fromConclusionGoalIndex :: Map Int [Step a c v]
      graph_fromConclusionGoalIndex =
        ss
          & (`foldl` Map.empty)
            ( flip \step ->
                Map.alter
                  (maybe (Just [step]) (Just . (step :)))
                  (step.goal.freshGoalIndex & fromMaybe (-1))
            )
      -- start from config goals
      go :: Int -> Doc
      go i = case graph_fromConclusionGoalIndex Map.!? i of
        Nothing -> div "Invalid" ["unknown goal index:" <+> pPrintEscaped i]
        Just [] -> error "impossible"
        Just ss'@(s0 : _) ->
          div
            "StepNode"
            [ div "goal" [renderGoal s0.goal],
              div "separator" [],
              div "steps" $
                ss' <&> \s ->
                  div "Step" $
                    [ div "rule" [renderRuleName s.rule.name],
                      div "subgoals" . fmap (go . (\g -> g.freshGoalIndex & fromMaybe (-1))) $ s0.subgoals
                    ]
            ]
   in div "Steps" . fmap go . (\gs -> [0 .. length gs]) $ cfg.goals

renderRuleName :: RuleName -> Doc
renderRuleName = div "RuleName" . pure . pPrintEscaped

renderAtom :: (Pretty a, Pretty c, Pretty v) => Atom a c v -> Doc
renderAtom a = div "Atom" (div "AtomName" [pPrintEscaped a.name] : fmap renderExpr a.args)

renderRule :: (Pretty v, Pretty c, Pretty a) => Rule a c v -> Doc
renderRule r =
  div "Rule" $
    [ div "name" [div "RuleName" [pPrintEscaped r.name]],
      div "hyps" . pure . renderList . fmap renderHyp $ r.hyps,
      div "conc" . pure . renderAtom $ r.conc
    ]

renderHyp :: (Pretty a, Pretty c, Pretty v) => Hyp a c v -> Doc
renderHyp (GoalHyp g) = div "GoalHyp" [renderGoal g]

escapeHtml :: String -> String
escapeHtml = concatMap escapeChar
  where
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '&' = "&amp;"
    escapeChar '"' = "&quot;"
    escapeChar '\'' = "&apos;"
    escapeChar c = [c]

pPrintEscaped :: (Pretty a) => a -> Doc
pPrintEscaped = text . escapeHtml . prettyShow
