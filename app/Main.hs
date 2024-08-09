module Main where

import NaturalCalculator

interactLines :: (String -> String) -> IO ()
interactLines f = do
  input <- getLine
  print $ f input
  interactLines f

showResult :: (Show a) => Either ParseError a -> String
showResult result = case result of
  Left errorReason -> show errorReason
  Right answer -> show answer

calculatorMain :: IO ()
calculatorMain = do
  putStrLn "Welcome to the Calculator!"
  putStrLn "Enter an expression to calculate the answer:"
  interactLines (showResult . (eval :: String -> Either ParseError Integer))

main :: IO ()
main = calculatorMain
