module Main where

import NaturalCalculator
import Data.Decimal

interactLines :: (String -> String) -> IO ()
interactLines f = do
  input <- getLine
  putStrLn $ f input
  interactLines f

showResult :: (Show a) => Either ParseError (Either EvaluationError a) -> String
showResult result = case result of
  Left errorReason -> show errorReason
  Right (Left errorReason) -> show errorReason
  Right (Right answer) -> show answer

calculatorMain :: IO ()
calculatorMain = do
  putStrLn "Welcome to the Calculator!"
  putStrLn "Enter an expression to calculate the answer:"
  interactLines (showResult . (eval :: String -> Either ParseError (Either EvaluationError Decimal)))

main :: IO ()
main = calculatorMain
