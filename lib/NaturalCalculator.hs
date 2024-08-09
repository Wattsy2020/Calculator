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
    EvaluationError (..),
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

data EvaluationError
  = DivisionByZero
  deriving (Show, Eq)

-- lexer, convert expression into tokens
data Op
  = Add
  | Multiply
  | Subtract
  | Divide
  deriving (Show, Eq)

data Token
  = Digit Int
  | Operator Op
  | OpenParen
  | CloseParen
  | DecimalPoint
  deriving (Show, Eq)

data Expression a
  = Value a
  | Expression (Expression a) Op (Expression a)
  deriving (Show, Eq)

readDigit' :: Char -> Either ParseError Int
readDigit' char = case readDigit char of
  Nothing -> Left (InvalidDigit char)
  Just digit -> Right digit

lexChar :: Char -> Either ParseError Token
lexChar '(' = Right OpenParen
lexChar ')' = Right CloseParen
lexChar '.' = Right DecimalPoint
lexChar '+' = Right (Operator Add)
lexChar '*' = Right (Operator Multiply)
lexChar '-' = Right (Operator Subtract)
lexChar '/' = Right (Operator Divide)
lexChar other = fmap Digit (readDigit' other)

lexExpression :: String -> Either ParseError [Token]
lexExpression = mapM lexChar . filter (/= ' ')

-- parse into abstract syntax tree
data ContinueInstruction = Continue | Stop

hasHighPrecedence :: Op -> Bool
hasHighPrecedence Multiply = True
hasHighPrecedence Divide = True
hasHighPrecedence Add = False
hasHighPrecedence Subtract = False

isNumericToken :: Token -> Bool
isNumericToken (Digit _) = True
isNumericToken DecimalPoint = True
isNumericToken _ = False

-- convert operation to string
serializeOp :: Op -> String
serializeOp Add = "+"
serializeOp Subtract = "-"
serializeOp Multiply = "*"
serializeOp Divide = "/"

serializeToken :: Token -> String
serializeToken (Digit digit) = show digit
serializeToken DecimalPoint = "."
serializeToken (Operator op) = serializeOp op
serializeToken OpenParen = "("
serializeToken CloseParen = ")"

-- combine all the starting tokens that are digits into a number, also returning remaining tokens
parseDigits :: (Read a, Fractional a) => [Token] -> (a, [Token])
parseDigits tokens = let (numericTokens, remaining) = span isNumericToken tokens in
  (read $ concatMap serializeToken numericTokens, remaining)

-- stop parsing the next tokens when encountering a stop instruction (started by the close bracket)
handleContinuation :: (Read a, Fractional a) => Expression a -> [Token] -> ContinueInstruction -> Either ParseError (Expression a, [Token], ContinueInstruction)
handleContinuation newExpr remainingTokens continueInstruction = case continueInstruction of
  Stop -> Right (newExpr, remainingTokens, Stop) -- pass the stop instruction until getting to the open paren which will realise we're going out of a sub-xpression
  Continue -> parseExpression' (Just newExpr) False remainingTokens

parseBrackets :: (Read a, Fractional a) => Maybe (Expression a) -> Bool -> [Token] -> Either ParseError (Expression a, [Token], ContinueInstruction)
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
parseExpression' :: (Read a, Fractional a) => Maybe (Expression a) -> Bool -> [Token] -> Either ParseError (Expression a, [Token], ContinueInstruction)
parseExpression' (Just expr) _ [] = Right (expr, [], Stop)
parseExpression' Nothing highPrecedence remaining@(Digit _ : _) =
  let (number, remainingTokens) = parseDigits remaining
   in if highPrecedence -- if highPrecedence then return immediately and let the expression with higher precedence evaluate the remaining tokens
        then Right (Value number, remainingTokens, Continue)
        else parseExpression' (Just $ Value number) highPrecedence remainingTokens
parseExpression' (Just leftExpr) _ (Operator op : remaining) = do
  (rightExpr, remainingTokens, continueInstruction) <- parseExpression' Nothing (hasHighPrecedence op) remaining
  let operatorExpr = Expression leftExpr op rightExpr
   in handleContinuation operatorExpr remainingTokens continueInstruction
parseExpression' Nothing _ (Operator Subtract : remaining) = do
  (rightExpr, remainingTokens, continueInstruction) <- parseExpression' Nothing True remaining
  let operatorExpr = Expression (Value 0) Subtract rightExpr
   in handleContinuation operatorExpr remainingTokens continueInstruction
-- reset precedence inside brackets, it's a separate operation
-- also, tell the consumer of this bracketed expression to continue parsing, after the close paren told the inner sub-expression to stop parsing
parseExpression' Nothing _ (OpenParen : remaining) = parseBrackets Nothing False remaining
parseExpression' (Just expr) _ (CloseParen : remaining) = Right (expr, remaining, Stop)
-- report incorrectly formed expressions
parseExpression' Nothing _ (Operator _ : _) = Left (InvalidExpression "no previous expression for the operator")
parseExpression' (Just _) _ (Digit _ : _) = Left (InvalidExpression "digit shouldn't have a previous expression")
parseExpression' (Just _) _ (OpenParen : _) = Left (InvalidExpression "open parenthesis shouldn't have a previous expression")
parseExpression' _ _ (DecimalPoint: _) = Left (InvalidExpression "decimal point wasn't preceeded by a number")
parseExpression' Nothing _ (CloseParen : _) = Left UnmatchingParenthesis
parseExpression' Nothing _ [] = Left EmptyExpression

handleParseExpression :: (Read a, Fractional a) => (Expression a, [Token], ContinueInstruction) -> Either ParseError (Expression a)
handleParseExpression result = case result of
  (expr, [], _) -> Right expr
  -- handle when there are leftover tokens, which can happen if there is a top level bracket like (1)+1
  (expr, remaining, _) -> parseExpression' (Just expr) False remaining >>= handleParseExpression

parseExpression :: (Read a, Fractional a) => [Token] -> Either ParseError (Expression a)
parseExpression = (>>= handleParseExpression) . parseExpression' Nothing False

readExpression :: (Read a, Fractional a) => String -> Either ParseError (Expression a)
readExpression = (>>= parseExpression) . lexExpression

-- evaluate an operation on its arguments
evalOp :: (Fractional a) => Op -> a -> a -> a
evalOp Add = (+)
evalOp Multiply = (*)
evalOp Subtract = (-)
evalOp Divide = (/)

-- evaluate an expression
evalExpression :: (Eq a, Fractional a) => Expression a -> Either EvaluationError a
evalExpression (Value result) = Right result
evalExpression (Expression left Divide right) = do
  rightResult <- evalExpression right
  if rightResult == 0 then Left DivisionByZero else flip (evalOp Divide) rightResult <$> evalExpression left
evalExpression (Expression left op right) = evalOp op <$> evalExpression left <*> evalExpression right

-- convert expression to string
serializeExpression :: (Show a) => Expression a -> String
serializeExpression (Value result) = show result
serializeExpression (Expression leftExpr op rightExpr) =
  "(" <> serializeExpression leftExpr <> ")" <> serializeOp op <> "(" <> serializeExpression rightExpr <> ")"

-- parse and evaluate a string expression
eval :: (Read a, Eq a, Fractional a) => String -> Either ParseError (Either EvaluationError a)
eval = fmap evalExpression . readExpression
