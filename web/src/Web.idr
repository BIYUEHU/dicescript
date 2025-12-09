module Web

import Data.List
import Web.Dom
import Web.Isx2
import Web.Es
import Examples
import Dice.Evaluator
import Dice.Parser
import Dice.Ast
import Dice.Value
import Dice.Utils

handleRuningExpr : String -> IO (String, String)
handleRuningExpr source = case parse source of
  Left err => pure ("result error", "Parse Error: " ++ err)
  Right ast => do
      Right val <- evaluate ast
        | Left err => pure ("result error", "Evaluation Error: " ++ err)
      pure ("result", "Result: " ++ show val)

setupEventHandlers : IO ()
setupEventHandlers = do
  buttons <- querySelectorAll ".btn-run"
  for_ buttons $ \btn => do
    Just id <- getAttribute btn "data-run-id"
      | Nothing => printLn "Failed to register event listener: No data-run-id found"
    addEventListener btn "click" $ do
      Just input <- querySelector $ "[data-input-id='" ++ id ++ "']"
        | Nothing => pure ()
      Just val <- getElemValue input
        | Nothing => pure ()
      Just resultBox <- querySelector $ "[data-result-id='" ++ id ++ "']"
        | Nothing => pure ()
      (cls, text) <- handleRuningExpr val
      setAttribute resultBox "class" cls
      setTextContent resultBox text
      pure ()

page : ISX
page = div ["class" .= "container"] [
    section ["class" .= "intro"] [
      h1 [] ["🎲 Dice Script Playground"],
      p [] ["A safe and fast dice expression evaluator base on Idris2. And DiceScript is a superset of Mathematical (Arithmetic) Expression, it supports many advanced features"],
      p [] ["Try rolling some dice using standard RPG notation."],
      ul [] [
          li [] [ text "Project links: ", a [
            "href" .= "https://github.com/BIYUEHU/dicescript",
            "target" .= "_blank"
            ] ["BIYUEHU/dicescript"] ],
          li [] [ text "DiceScript Guide: ", a [
            "href" .= "https://github.com/dice-project/dicescript/blob/main/docs/syntax.md",
            "target" .= "_blank"
            ] ["👉 Here"]]
        ]
    ],
    div ["class" .= "expr-blocks"] exampleBlocks
  ]

export
partial
main : IO ()
main = do
  Just container <- querySelector ".idris-dice-script-container"
    | Nothing => putStrLn "Container not found"
  renderToDOM container page
  setupEventHandlers
  putStrLn "Demo blocks generated!"
