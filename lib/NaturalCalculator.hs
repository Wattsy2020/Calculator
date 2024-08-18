-- A calculator for natural expressions
module NaturalCalculator ( eval, ParseError(..), EvaluationError(..) )
where

import Expression ( ParseError(..) )
import Evaluator ( EvaluationError(..), evalExpression )
import Parser ( readExpression )
import Control.Monad ((<=<))

-- parse and evaluate a string expression
eval :: (Read a, Eq a, Floating a) => String -> Either ParseError (Either EvaluationError a)
eval = fmap evalExpression . readExpression
