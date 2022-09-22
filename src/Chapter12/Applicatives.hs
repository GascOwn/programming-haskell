{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use replicateM" #-}
module Chapter12.Applicatives where

{- 
    Applicatives abstract the idea of allowing functions with any number of 
    arguments to be mapped.
    Using the same idea of currying for which (a -> b) -> f a -> f b is a 
    generalised form of function application (a -> b) -> a -> b, an fmap for
    functions with any desired number of arguments can be constructed in terms
    of two basic functions:

    pure:: a -> f a, which converts a value of type a in a structure of type f a
    (<*>) :: f (a -> b) -> f a -> f, another generalised form of function
    application in which function, value, and result are all contained in structures
    of type f. <*> is left associative, so 

    g <*> x <*> y <*> z == ((g <*> x) <*> y) <*> z 

    Functors that support pure and <*> are called Applicative Functors, or 
    Applicatives for short.

    class Functor f => Applicative f where
        pure :: a -> f a 
        (<*>) :: f (a -> b) -> f a -> f b

    Maybe is a good example of an Applicative 

    instance Applicative Maybe where 
        pure = Just 
        Nothing <*> _ = Nothing
        (Just g) <*> mx = fmap g mx 
-}

maybeApp :: Maybe Integer
maybeApp = pure (+) <*> Just 1 <*> Just 2 -- Just 3

nothingApp :: Maybe Integer
nothingApp =  pure (+) <*> Nothing <*> Just 2 -- Nothing

{-
    The List applicative takes a list of functions and a list of values, then
    applies each function to each value and combines them in a list

    instance Applicative [] where
        pure x = [x]
        gs <*> xs = [g x | g <- gs, x <- xs]
-}

listApp :: [Integer]
listApp = pure (*) <*> [1,2] <*> [3,4] -- [1*2,1*3,2*3,2*4] -> [3,4,6,8]

-- the below is equal to prod xs ys = pure (*) <*> xs <*> ys
prods :: [Int] -> [Int] -> [Int]
prods xs ys = (*) <$> xs <*> ys -- <$> is fmap

{-
    The IO type is also an Applicative:
    
    instance Applicative IO where
        pure = return
        mg <*> mx = do {g <- mg; x <- mx; return (g x)}
    
    Pure is simply return, while <*> applies an inpure function to an impure
    argument to give an impure result
-}

getChars :: Int -> IO String
getChars 0 = return []
getChars n = (:) <$> getChar <*> getChars (n-1)

{-
    Applicatives concern programming with effects. The <*> allows writing programs
    in an applicative style in which functions are applied to arguments, with
    the difference that the arguments may also have effects, such as failure,
    input/output, multiple ways of succeding, Applicatives can be viewed as 
    abstracting the idea of applying pure functions to effectful arguments, with
    the form of effects defined by the nature of the underlying functor.
    They also allow the writing of generic functions that can be used with any
    applicative, such as sequenceA:

    sequenceA :: Applicative f => [f a] -> f [a]
    sequenceA [] = pure []
    sequenceA (x:xs) = pure (:) <*> x <*> sequenceA xs

    Which allows a better version of getChars, which uses replicate to repeat
    the basic action getChar a number of times, then executes the sequence:
-}

getChars' :: Int -> IO String 
getChars' n = sequenceA (replicate n getChar)