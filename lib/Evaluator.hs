module Evaluator (EvaluationError(..), evalExpression) where

import Expression ( Expression(..), Op(..) )
import FunctorUtils ( ($>) )

data EvaluationError
  = DivisionByZero
  deriving (Show, Eq)

-- evaluate an operation on its arguments
evalOp :: (Floating a) => Op -> a -> a -> a
evalOp Add = (+)
evalOp Multiply = (*)
evalOp Subtract = (-)
evalOp Divide = (/)
evalOp Power = (**)

-- evaluate an expression
evalExpression :: (Eq a, Floating a) => Expression a -> Either EvaluationError a
evalExpression (Value result) = Right result
evalExpression (Expression left Divide right) = do
  rightResult <- evalExpression right
  if rightResult == 0 then Left DivisionByZero else evalOp Divide <$> evalExpression left $> rightResult
evalExpression (Expression left op right) = evalOp op <$> evalExpression left <*> evalExpression right
