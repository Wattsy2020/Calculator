module Console (interactLines) where

import System.Console.ANSI
import System.IO
import Data.Sequence qualified as Seq
import Data.Foldable ( Foldable(toList) )

data Key = 
  UpArrow
  | DownArrow
  | LeftArrow
  | RightArrow
  | Delete
  | Char Char
  deriving Show

type SeqString = Seq.Seq Char

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
    '\BS' -> return Delete
    '\DEL' -> return Delete
    '\^[' -> do
      char2 <- getChar
      char3 <- getChar
      return $ case (char2, char3) of
        ('[', 'A') -> UpArrow
        ('[', 'B') -> DownArrow
        ('[', 'C') -> RightArrow
        ('[', 'D') -> LeftArrow
        _ -> Char char2 -- should handle the other escape sequences
    _ -> return $ Char char

-- clear the line and reset the cursor position
resetLine :: IO ()
resetLine = clearLine >> setCursorColumn 0

-- reset the line to a string
resetLineTo :: String -> IO ()
resetLineTo string = resetLine >> putStr string

-- convert SeqString to a string
convert :: SeqString -> String
convert = toList

-- show an input and continue the input loop
showInput :: (String -> String) -> SeqString -> [SeqString] -> Int -> Int -> IO ()
showInput f input previousInputs inputIdx colIdx = do 
  resetLineTo $ convert input
  setCursorColumn colIdx
  interactLines' f input previousInputs inputIdx colIdx

-- show a previous input given the idx of that input
showInputIdx :: (String -> String) -> SeqString -> [SeqString] -> Int -> Int -> IO ()
showInputIdx f input previousInputs inputIdx prevIdx = case previousInputs !? inputIdx of
  Nothing -> showInput f input previousInputs prevIdx (length input) -- reset the cursor to point to the end of the line
  Just selectedInput -> showInput f selectedInput previousInputs inputIdx (length selectedInput)

interactLines' :: (String -> String) -> SeqString -> [SeqString] -> Int -> Int -> IO ()
interactLines' f input previousInputs inputIdx colIdx = do
  key <- getKey
  case key of
    UpArrow -> showInputIdx f input previousInputs (inputIdx + 1) inputIdx 
    DownArrow -> showInputIdx f input previousInputs (inputIdx - 1) inputIdx
    LeftArrow -> showInput f input previousInputs inputIdx (max (colIdx - 1) 0)
    RightArrow -> showInput f input previousInputs inputIdx (min (colIdx + 1) (length input))
    Delete -> if colIdx == 0
      then interactLines' f input previousInputs inputIdx colIdx
      else showInput f (Seq.deleteAt (colIdx - 1) input) previousInputs inputIdx (colIdx - 1)
    Char '\n' -> do
      putStrLn $ f $ convert input
      interactLines' f Seq.empty (input : previousInputs) (-1) 0
    Char char -> showInput f (Seq.insertAt colIdx char input) previousInputs (-1) (colIdx + 1)

-- Start a simple CLI program
-- Each line of input is passed to f, the output of f is then printed
-- It supports using the Up and Down arrow keys to select previous inputs
-- And using the Left and Right arrow keys, and the Delete key, to edit the current input 
interactLines :: (String -> String) -> IO ()
interactLines f = do 
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  interactLines' f Seq.Empty [] (-1) 0
