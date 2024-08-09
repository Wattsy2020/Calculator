module FunctorUtils where

-- apply a function in a functor to a normal value
infixl 4 $>
($>) :: Functor f => f (a -> b) -> a -> f b
($>) func value = fmap ($ value) func
