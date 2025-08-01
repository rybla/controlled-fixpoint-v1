{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}

module ControlledFixpoint.Html where

import ControlledFixpoint.Engine
import ControlledFixpoint.Grammar
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.PrettyPrint (Doc, doubleQuotes, nest, text, vcat, (<+>), (<>))
import Text.PrettyPrint.HughesPJClass (Pretty, prettyShow)
import Utility (comps)
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

renderTrace :: forall a c v. (Pretty a, Pretty c, Pretty v) => Config a c v -> Trace a c v -> Doc
renderTrace cfg tr = div "Trace" $ cfg.goals <&> \g -> renderTraceNode g.goalIndex
  where
    renderTraceNode :: GoalIndex -> Doc
    renderTraceNode gi =
      case tr.traceSteps Map.!? gi of
        Nothing ->
          case tr.traceGoals Map.!? gi of
            Nothing -> div "invalid" ["unknown goal index:" <+> pPrintEscaped gi]
            Just g ->
              div "TraceNode" $
                [div "failed" [renderGoal g]]
        Just steps ->
          div "TraceNode" $
            steps <&> \step ->
              div "TraceStep" $
                [ div "goal" [renderGoal step.goal],
                  div "rule" [renderRuleName step.rule.name],
                  div "sigma" [renderSubst step.sigma],
                  if null step.subgoals
                    then div "solved" ["solved"]
                    else div "substeps" $ step.subgoals <&> renderTraceNode . goalIndex
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
    [ div "sidebar" $
        [ div "activeGoals" . pure . renderList . fmap renderGoal $ env.activeGoals,
          div "suspendedGoals" . pure . renderList . fmap renderGoal $ env.suspendedGoals,
          div "failedGoals" . pure . renderList . fmap renderGoal $ env.failedGoals,
          div "sigma" . pure . renderSubst $ env.sigma
        ],
      div "main" $
        [div "steps" . pure . renderStepsGraph cfg . reverse $ env.stepsRev]
    ]

renderGoal :: (Pretty v, Pretty c, Pretty a) => Goal a c v -> Doc
renderGoal g =
  div "Goal" $
    [ div "goalIndex" . pure . renderGoalIndex $ g.goalIndex,
      div "atom" . pure . renderAtom $ g.atom,
      div "opts" . fmap renderGoalOpt . Set.toList $ g.opts
    ]

renderGoalIndex :: GoalIndex -> Doc
renderGoalIndex = maybe mempty \i -> div "GoalIndex" [pPrintEscaped i]

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

renderStepsGraph :: forall a c v. (Pretty v, Pretty c, Pretty a) => Config a c v -> [Step a c v] -> Doc
renderStepsGraph cfg ss =
  let graph_fromConcGoalIndex :: Map GoalIndex (Either (Goal a c v) [Step a c v])
      graph_fromConcGoalIndex =
        ss
          & (\f -> foldl f Map.empty)
            ( flip \s ->
                comps
                  [ Map.alter
                      ( -- append Right step to whatever is there (overriding a Left goal)
                        \case
                          Nothing -> Just (Right [s])
                          Just (Left _) -> Just (Right [s])
                          Just (Right ss') -> Just (Right (ss' ++ [s]))
                      )
                      s.goal.goalIndex,
                    (\f y -> foldl f y s.subgoals)
                      ( flip \g ->
                          Map.alter
                            ( -- insert Left goal if nothing for that goal index is there already
                              \case
                                Nothing -> Just (Left g)
                                x -> x
                            )
                            g.goalIndex
                      )
                  ]
            )

      -- start from config goals
      go :: GoalIndex -> Doc
      go i = case graph_fromConcGoalIndex Map.!? i of
        Nothing -> div "Invalid" ["unknown goal index:" <+> pPrintEscaped i]
        Just (Left g) ->
          div "StepNode" $
            [ div "goal" [renderGoal g],
              div "separator" [],
              div "substeps" [div "dead-end-message" ["dead end"]]
            ]
        Just (Right []) -> error "impossible"
        Just (Right ss'@(s0 : _)) ->
          div "StepNode" $
            [ div "goal" [renderGoal s0.goal],
              div "separator" [],
              div "substeps" $
                ss' <&> \s ->
                  div "Step" $
                    [ div "rule" [renderRuleName s.rule.name],
                      div "subgoals" . fmap (go . goalIndex) $ s0.subgoals
                    ]
            ]
   in div "StepsGraph" . fmap (go . goalIndex) $ cfg.goals

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
