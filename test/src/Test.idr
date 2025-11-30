module Test

import Test.Parser
import Test.Evaluator

main : IO ()
main = do
  putStrLn "\n"
  putStrLn "\x1b[1m\x1b[35mTesting Parser..."
  testParser
  putStrLn "\x1b[1m\x1b[35mTesting Evaluator..."
  testEvaluator
