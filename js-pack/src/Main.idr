module Main

import Exports
import JsValue

import Dice.Ast
import Dice.Parser
import Dice.Evaluator
import Dice.Lexer
import Dice.Token
import Dice.Utils
import Dice.Value
import Dice.Internal
import Dice.Random


runDice : String -> IO JSValue
runDice code = do
  case parse code of
    Left err => throw (IO JSValue) err
    Right ast => do
      Right result <- evaluate ast
        | Left err => throw (IO JSValue) err
      pure $ valueToJS result




main : IO ()
main = do
  isNodeEnvironment <- isNodeEnvironment
  if isNodeEnvironment then do
      exportCommonJs "runDice" runDice
      exportCommonJs "privEvaluate" evaluate
      exportCommonJs "privParse" parse
    else do
      exportGlobalJs "runDice" runDice
      exportGlobalJs "privEvaluate" evaluate
      exportGlobalJs "privParse" parse
