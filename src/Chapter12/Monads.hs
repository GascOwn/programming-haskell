module Chapter12.Monads where

{- 
    The applicative style restricts to applying pure functions to effectful 
    arguments. A monad captures another aspect of effectful programming, and can
    be defined thus:

    class Applicative m => Monad m where
        return :: a -> m a 
        (>>=) :: m a -> (a -> m b) -> m b

        return = pure
    
    Essentialy, return is the same as pure, and >>= (called "bind") takes the 
    value of type a inside of a monad of type m, feeds it to a function that 
    takes a value of that type, which gives back a value of type b wrapped in 
    the same monad. An example can be made with Maybe:

    instance Monad Maybe where
        Nothing >>= _ = Nothing
        (Just x) >>= f = f x

    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b

    This means that if one of the two values if Nothing, Nothing is automatically
    returned, thus encapsulating the notion of potential failure.
-}

data Expr = Val Int | Div Expr Expr

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

evalBind :: Expr -> Maybe Int
evalBind (Val n) = Just n
evalBind (Div x y) = evalBind x >>= \n ->
        evalBind y >>= \m -> safediv n m

{-
    The second part of evalBind states that x is evaluated and bound to n, then
    y is evaluated and abound to m. The two values are then passed to safediv as
    arguments. There is a more readable way of writing this sequence, which is 
    do notation:
-}

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y )= do
    n <- eval x
    m <- eval y
    safediv n m

{-
    Lists can be made into Monads as well:

    (>>=) :: [a] -> (a -> [b]) -> [b]

    instance Monad [] where 
        xs >>= f = [y | x <- xs, y <- f x]
    
    The above applies a function f to each of the results in the list xs,
    collecting the resulting values in list y. It provides a means of sequencing
    expressions that may produce multiple result. 
-}

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do
    x <- xs
    y <- ys
    return (x, y)

pairsApplicative :: [a] -> [b] -> [(a,b)]
pairsApplicative xs ys = (, ) <$> xs <*> ys

pairsComp :: [a] -> [b] -> [(a, b)]
pairsComp xs ys = [(x, y) | x <- xs, y <- ys]

{- 
    The do notation can be used with any monad, while the comprehension notation
    is exclusive to lists.
-}

