module Expression 
  ( lexExpression,
    serializeExpression,
    serializeToken,
    ParseError(..),
    Op (..),
    Expression (..),
    Token (..),
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
  = Add
  | Multiply
  | Subtract
  | Divide
  | Power
  deriving (Show, Eq)

data Token
  = Digit Int
  | Operator Op
  | OpenParen
  | CloseParen
  | DecimalPoint
  | Exponent
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
lexChar 'e' = Right Exponent
lexChar '+' = Right (Operator Add)
lexChar '*' = Right (Operator Multiply)
lexChar '-' = Right (Operator Subtract)
lexChar '/' = Right (Operator Divide)
lexChar '^' = Right (Operator Power)
lexChar other = fmap Digit (readDigit' other)

-- read string into tokens
lexExpression :: String -> Either ParseError [Token]
lexExpression = mapM lexChar . filter (/= ' ')

-- convert operation to string
serializeOp :: Op -> String
serializeOp Add = "+"
serializeOp Subtract = "-"
serializeOp Multiply = "*"
serializeOp Divide = "/"
serializeOp Power = "^"

serializeToken :: Token -> String
serializeToken (Digit digit) = show digit
serializeToken DecimalPoint = "."
serializeToken (Operator op) = serializeOp op
serializeToken OpenParen = "("
serializeToken CloseParen = ")"
serializeToken Exponent = "e"

-- convert expression to string
serializeExpression :: (Show a) => Expression a -> String
serializeExpression (Value result) = show result
serializeExpression (Expression leftExpr op rightExpr) =
  "(" <> serializeExpression leftExpr <> ")" <> serializeOp op <> "(" <> serializeExpression rightExpr <> ")"
