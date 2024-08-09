-- A calculator for natural expressions
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <=<" #-}
module NaturalCalculator
  ( eval,
    evalExpression,
    parseExpression,
    readExpression,
    serializeExpression,
    lexExpression,
    ParseError,
    Op (..),
    Expression (..),
    Token,
  )
where

import StringUtils (readDigit)

data ParseError
  = InvalidDigit Char
  | InvalidExpression String
  | UnmatchingParenthesis
  | EmptyExpression
  deriving (Show, Eq)

-- lexer, convert expression into tokens
data Op
  = Plus
  | Multiply
  | Minus
  deriving (Show, Eq)

data Token a
  = Digit a
  | Operator Op
  | OpenParen
  | CloseParen
  deriving (Show, Eq)

data Expression a
  = Value a
  | Expression (Expression a) Op (Expression a)
  deriving (Show, Eq)

readDigit' :: (Num a) => Char -> Either ParseError a
readDigit' char = case readDigit char of
  Nothing -> Left (InvalidDigit char)
  Just digit -> Right digit

lexChar :: (Num a) => Char -> Either ParseError (Token a)
lexChar '(' = Right OpenParen
lexChar ')' = Right CloseParen
lexChar '+' = Right (Operator Plus)
lexChar '*' = Right (Operator Multiply)
lexChar '-' = Right (Operator Minus)
lexChar other = fmap Digit (readDigit' other)

lexExpression :: (Num a) => String -> Either ParseError [Token a]
lexExpression = mapM lexChar . filter (/= ' ')

-- parse into abstract syntax tree
data ContinueInstruction = Continue | Stop

-- combine all the starting tokens that are digits into a number, also returning remaining tokens
parseDigits :: (Num a) => a -> [Token a] -> (a, [Token a])
parseDigits prev ((Digit value) : remaining) = parseDigits (10 * prev + value) remaining
parseDigits prev remaining = (prev, remaining)

-- stop parsing the next tokens when encountering a stop instruction (started by the close bracket)
handleContinuation :: (Num a) => Expression a -> [Token a] -> ContinueInstruction -> Either ParseError (Expression a, [Token a], ContinueInstruction)
handleContinuation newExpr remainingTokens continueInstruction = case continueInstruction of
  Stop -> Right (newExpr, remainingTokens, Stop) -- pass the stop instruction until getting to the open paren which will realise we're going out of a sub-xpression
  Continue -> parseExpression' (Just newExpr) False remainingTokens

parseBrackets :: (Num a) => Maybe (Expression a) -> Bool -> [Token a] -> Either ParseError (Expression a, [Token a], ContinueInstruction)
parseBrackets expr _ remaining = do
  (newExpr, remainingTokens, continueInstruction) <- parseExpression' expr False remaining
  case continueInstruction of
    Stop -> Right (newExpr, remainingTokens, Continue)
    --we might have already exited from inner brackets, and need to continue parsing the next tokens inside this bracket pair
    -- e.g. consider evaluating 8*((-3)+5)
    -- we also need to continue handling exits from further inner brackets, so we recurse
    -- e.g. consider evaluating "0-(((3)+(1))-8)"
    Continue -> parseBrackets (Just newExpr) False remainingTokens

-- parse an expression, given the expression up until this point
-- returns the remaining tokens after parsing the expression
-- todo: Make this return an `Either`
parseExpression' :: (Num a) => Maybe (Expression a) -> Bool -> [Token a] -> Either ParseError (Expression a, [Token a], ContinueInstruction)
parseExpression' (Just expr) _ [] = Right (expr, [], Stop)
parseExpression' Nothing highPrecedence (Digit value : remaining) =
  let (number, remainingTokens) = parseDigits value remaining
   in if highPrecedence -- if highPrecedence then return immediately and let the expression with higher precedence evaluate the remaining tokens
        then Right (Value number, remainingTokens, Continue)
        else parseExpression' (Just $ Value number) highPrecedence remainingTokens
parseExpression' (Just leftExpr) _ (Operator op : remaining) = do
  (rightExpr, remainingTokens, continueInstruction) <- parseExpression' Nothing (op == Multiply) remaining
  let operatorExpr = Expression leftExpr op rightExpr
   in handleContinuation operatorExpr remainingTokens continueInstruction
parseExpression' Nothing _ (Operator Minus : remaining) = do
  (rightExpr, remainingTokens, continueInstruction) <- parseExpression' Nothing True remaining
  let operatorExpr = Expression (Value 0) Minus rightExpr
   in handleContinuation operatorExpr remainingTokens continueInstruction
-- reset precedence inside brackets, it's a separate operation
-- also, tell the consumer of this bracketed expression to continue parsing, after the close paren told the inner sub-expression to stop parsing
parseExpression' Nothing _ (OpenParen : remaining) = parseBrackets Nothing False remaining
parseExpression' (Just expr) _ (CloseParen : remaining) = Right (expr, remaining, Stop)
-- report incorrectly formed expressions
parseExpression' Nothing _ (Operator _ : _) = Left (InvalidExpression "no previous expression for the operator")
parseExpression' (Just _) _ (Digit _ : _) = Left (InvalidExpression "digit shouldn't have a previous expression")
parseExpression' (Just _) _ (OpenParen : _) = Left (InvalidExpression "open parenthesis shouldn't have a previous expression")
parseExpression' Nothing _ (CloseParen : _) = Left UnmatchingParenthesis
parseExpression' Nothing _ [] = Left EmptyExpression

handleParseExpression :: (Num a) => (Expression a, [Token a], ContinueInstruction) -> Either ParseError (Expression a)
handleParseExpression result = case result of
  (expr, [], _) -> Right expr
  -- handle when there are leftover tokens, which can happen if there is a top level bracket like (1)+1
  (expr, remaining, _) -> parseExpression' (Just expr) False remaining >>= handleParseExpression

parseExpression :: (Num a) => [Token a] -> Either ParseError (Expression a)
parseExpression = (>>= handleParseExpression) . parseExpression' Nothing False

readExpression :: (Num a) => String -> Either ParseError (Expression a)
readExpression = (>>= parseExpression) . lexExpression

-- evaluate an operation on its arguments
evalOp :: (Num a) => Op -> a -> a -> a
evalOp Plus = (+)
evalOp Multiply = (*)
evalOp Minus = (-)

-- evaluate an expression
evalExpression :: (Num a) => Expression a -> a
evalExpression (Value result) = result
evalExpression (Expression left op right) = evalOp op (evalExpression left) (evalExpression right)

-- convert operation to string
serializeOp :: Op -> String
serializeOp Plus = "+"
serializeOp Minus = "-"
serializeOp Multiply = "*"

-- convert expression to string
serializeExpression :: (Show a) => Expression a -> String
serializeExpression (Value result) = show result
serializeExpression (Expression leftExpr op rightExpr) = 
  "(" <> serializeExpression leftExpr <> ")" <> serializeOp op <> "(" <> serializeExpression rightExpr <> ")"

-- parse and evaluate a string expression
eval :: (Num a) => String -> Either ParseError a
eval = fmap evalExpression . readExpression
