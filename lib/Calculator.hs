module Calculator where

-- Implement a reverse polish notation calculator for simple math operations
data Op = Plus | Minus | Multiply deriving (Show)

data Token a = Number a | Operator Op deriving (Show)

parseToken :: (Read a) => String -> Token a
parseToken "+" = Operator Plus
parseToken "-" = Operator Minus
parseToken "*" = Operator Multiply
parseToken number = Number (read number)

parseExpression :: (Read a) => String -> [Token a]
parseExpression = map parseToken . words

opToFunc :: (Num a) => Op -> (a -> a -> a)
opToFunc Plus = (+)
opToFunc Minus = (-)
opToFunc Multiply = (*)

evalToken :: (Num a) => Token a -> [a] -> Either String [a]
evalToken (Operator _) [] = Left "can't evaluate an operator on no arguments"
evalToken (Operator _) [_] = Left "can't evaluate an operator on one argument"
evalToken (Number num) [] = Right [num]
evalToken (Number num) stack = Right $ num : stack
evalToken (Operator op) (first : second : stack) = Right $ opToFunc op second first : stack

evalExpr2 :: (Num a) => [Token a] -> Either String [a]
evalExpr2 = foldl (\stack newToken -> stack >>= evalToken newToken) (Right [])

evalExpression :: (Num a) => [Token a] -> Either String a
evalExpression tokens =
  case foldl (\stack newToken -> stack >>= evalToken newToken) (Right []) tokens of
    Left errorStr -> Left errorStr
    Right [] -> Left "no tokens to evaluate"
    Right (result : _) -> Right result

evalExpr :: (Num a, Read a) => String -> Either String a
evalExpr = evalExpression . parseExpression

main :: IO ()
main = do
  print (evalExpr "10 4 3 + 2 * -" :: Either String Double)
  print (evalExpr "90 34 12 33 55 66 + * - + -" :: Either String Double)
  print (evalExpr "10 + 2" :: Either String Double) -- will show the error
