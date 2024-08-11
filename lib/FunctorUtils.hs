module FunctorUtils where

-- apply a function in a functor to a normal value
infixl 4 $>
($>) :: Functor f => f (a -> b) -> a -> f b
($>) func value = fmap ($ value) func

-- convert a tuple of applicatives to an applicative of a tuple
liftTuple :: Applicative f => (f a, f b) -> f (a, b)
liftTuple (item1, item2) = (,) <$> item1 <*> item2

liftFirst :: Functor f => (f a, b) -> f (a, b)
liftFirst (item1, elem2) = (,elem2) <$> item1
