module Console (interactLines) where 

import System.Console.ANSI
import System.IO

data Key = 
  UpArrow
  | DownArrow
  | Char Char

(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(x: xs) !? idx
  | idx == 0   = Just x
  | otherwise  = xs !? (idx - 1)

-- retrieve a single input, handling ascii escaped characters like up arrows
getKey :: IO Key
getKey = do
  char <- getChar
  case char of
    '\^[' -> do
      char2 <- getChar
      char3 <- getChar
      return $ case (char2, char3) of
        ('[', 'A') -> UpArrow
        ('[', 'B') -> DownArrow
        _ -> Char char3 -- should handle the other escape sequences
    _ -> return $ Char char

-- clear the line and reset the cursor position
resetLine :: IO ()
resetLine = clearLine >> setCursorColumn 0

showInputIdx :: (String -> String) -> String -> [String] -> Int -> Int -> IO ()
showInputIdx f input previousInputs inputIdx prevIdx = case previousInputs !? inputIdx of
  Nothing -> interactLines' f input previousInputs prevIdx
  Just selectedInput -> do
    resetLine
    putStr selectedInput
    interactLines' f selectedInput previousInputs inputIdx

interactLines' :: (String -> String) -> String -> [String] -> Int -> IO ()
interactLines' f input previousInputs inputIdx = do
  key <- getKey
  case key of
    UpArrow -> showInputIdx f input previousInputs (inputIdx + 1) inputIdx
    DownArrow -> showInputIdx f input previousInputs (inputIdx - 1) inputIdx
    Char '\n' -> do
      putStrLn $ f input
      interactLines' f "" (input : previousInputs) (-1)
    Char char -> interactLines' f (input ++ [char]) previousInputs (-1)

-- Start a simple CLI program
-- Each line of input is passed to f, the output of f is then printed
-- It also supports using the Up and Down arrow keys to select previous inputs
interactLines :: (String -> String) -> IO ()
interactLines f = do 
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  interactLines' f "" [] (-1)
