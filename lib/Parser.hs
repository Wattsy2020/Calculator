module Parser ( parseExpression, readExpression ) where

import Expression
import Text.Read (readMaybe)
import Control.Monad ((<=<))
import FunctorUtils (liftFirst, ($>))

isNumericToken :: Token -> Bool
isNumericToken (Digit _) = True
isNumericToken DecimalPoint = True
isNumericToken _ = False

-- read an integer, returning the remaining tokens
readInteger :: Int -> [Token] -> (Int, [Token])
readInteger prev ((Digit value) : remaining) = readInteger (10 * prev + value) remaining
readInteger prev remaining = (prev, remaining)

-- combine all the starting tokens that are digits into a number, also returning remaining tokens
parseDigits :: forall a. (Read a) => [Token] -> (Maybe a, [Token])
parseDigits tokens =
  let (numericTokens, remaining) = span isNumericToken tokens
      mantissaStr = concatMap serializeToken numericTokens in
  case remaining of
    (Exponent : Operator Subtract : (Digit value) : remaining2) -> let (exponent, remaining3) = readInteger value remaining2 in
      (readMaybe (mantissaStr ++ "e-" ++ show exponent), remaining3)
    (Exponent : Operator Add : (Digit value) : remaining2) -> let (exponent, remaining3) = readInteger value remaining2 in
      (readMaybe (mantissaStr ++ "e+" ++ show exponent), remaining3)
    _ -> (readMaybe mantissaStr, remaining)

-- precedence of an operator, high precedence operators must be evaluated before lower precedence operators
precedence :: Op -> Int
precedence Power = 2
precedence Multiply = 1
precedence Divide = 1
precedence Add = 0
precedence Subtract = 0

-- if an operator is left associative, i.e. a op b op c = (a op b) op c
isLeftAssociative :: Op -> Bool
isLeftAssociative Power = False
isLeftAssociative _ = True

-- apply all the operators to the expression stack, returning the new expression stack
evaluateStacks :: [Expression a] -> [Op] -> Either ParseError [Expression a]
evaluateStacks exprs [] = Right exprs
evaluateStacks (expr1 : expr2: remainingExprs) (op : remainingOps) = 
  evaluateStacks (Expression expr2 op expr1 : remainingExprs) remainingOps
evaluateStacks _ (_ : _) = Left (InvalidExpression "operation wasn't preceeeded by a number")

handleEvaluate :: [Expression a] -> [Op] -> [Token] -> Either ParseError (Expression a, [Token])
handleEvaluate exprs ops remaining = liftFirst (head <$> evaluateStacks exprs ops, remaining)

-- parse an expression using the shunting yard algorithm
-- returns the remaining tokens after parsing the expression
parseExpression' :: (Read a, Num a) => [Expression a] -> [Op] -> [Token] -> Either ParseError (Expression a, [Token])
parseExpression' [] _ [] = Left EmptyExpression
parseExpression' exprs ops [] = handleEvaluate exprs ops []
parseExpression' exprs ops remaining@(Digit _ : _) =
  case parseDigits remaining of
    (Just number, remainingTokens) -> parseExpression' (Value number : exprs) ops remainingTokens
    (Nothing, _) -> Left $ InvalidExpression "Number was incorrectly formatted"
-- treat (-x) as (0 - x)
parseExpression' [] [] (Operator Subtract : remaining) = parseExpression' [Value 0] [Subtract] remaining
parseExpression' exprs@(expr: _) [] (Operator op : remaining) = parseExpression' exprs [op] remaining
parseExpression' [] _ (Operator _ : _) = Left (InvalidExpression "operation wasn't preceeeded by a number")
parseExpression' exprs ops (Operator op : remainingTokens) = 
  let isLeftAssociative' = isLeftAssociative op
      precNew = precedence op
      (opsToEvaluate, remainingOps) = span (\op -> precNew < precedence op || (isLeftAssociative' && precNew == precedence op)) ops
   in evaluateStacks exprs opsToEvaluate >>= \exprs -> parseExpression' exprs (op : remainingOps) remainingTokens
-- recursively parse bracketed expressions
parseExpression' exprs ops (OpenParen : remaining) = do
  (expression, remainingTokens) <- parseExpression' [] [] remaining
  parseExpression' (expression : exprs) ops remainingTokens
parseExpression' [] _ (CloseParen : _) = Left UnmatchingParenthesis
parseExpression' exprs ops (CloseParen : remaining) = handleEvaluate exprs ops remaining
parseExpression' _ _ (DecimalPoint : _) = Left (InvalidExpression "decimal point wasn't preceeded by a number")
parseExpression' _ _ (Exponent : _) = Left (InvalidExpression "exponent wasn't preceeded by a number")

parseExpression :: (Read a, Num a) => [Token] -> Either ParseError (Expression a)
parseExpression = (fst <$>) . parseExpression' [] []

readExpression :: (Read a, Num a) => String -> Either ParseError (Expression a)
readExpression = parseExpression <=< lexExpression
