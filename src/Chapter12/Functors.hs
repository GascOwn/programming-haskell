{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}

module Chapter12.Functors where

-- Functors capture the generic notion of mapping

{- 
    The following functions are defined with the empty list mapping to itself
    while the non-empty list to some function applied to the head of the list
    and the result of recursively processing the tail. The function is applied to
    each member of the list
-}

inc :: [Int] -> [Int]
inc [] = []
inc (n:ns) = n + 1 : inc ns

sqr :: [Int] -> [Int]
sqr [] = []
sqr (n:ns) = n ^ 2 : sqr ns

-- The two above patterns can be abstracted with the use of the map function

inc' :: [Integer] -> [Integer]
inc' = map (+1)

sqr' :: [Integer] -> [Integer]
sqr' = map (^2)

{- 
    The idea of mapping a function over each element of a data structure ca be 
    abstracted to a other parametrised types. The class of types that support
    such mapping function are called Functors. The concept is captured by the 
    following class declaration:

    class Functor f where 
        fmap :: (a -> b) -> f a -> f b 
    
    Which means that for a parametrised type f to be an instance of Functor,
    it must implement fmap. Fmap takes a function of type (a -> b) and a structure
    of type f a whose elements have type a, and applies the function to each 
    element to return a structure of type f b whose elements are of type b.
-}

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
    fmap g (Leaf x) = Leaf (g x)
    fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

areNodesEven :: Tree Int -> Tree Bool
areNodesEven = fmap even 

{- 

    With most Functors, f is a data structure (called a container type) that 
    contains elements of type a, and fmap applies a given function to each element.
    Not all of them work that way: the IO type isn't a normal container type,
    its values represent input/output actions, but it can be made into a functor

    instance Functor IO where
        fmap g mx = do {x <- mx; return (g x)}
    
    In this case, fmap applies a function to the result of the action:

    fmap show(return True)

    would give "True"

    fmap can be used to process elements of any functorial structure, so the name
    can be used for functions that are the same, but applied to different 
    structures. We can define generic functions that work with any functor: 
-}

inc'' :: Functor f => f Int -> f Int
inc'' = fmap (+1)

justInc :: Maybe Int 
justInc = inc'' (Just 1)

listInc :: [Int]
listInc = inc'' [1,2,3,4,5,6]

treeInc :: Tree Int
treeInc = inc'' (Node (Leaf 1) 2 (Leaf 3))

{- 
    For any parametrised type, there is only one implementation of fmap that 
    satisfies functor laws:
    - fmap id = id 
    - fmap (f . g) = fmap f . fmap g
    That is, fmap respects identity and composition
-} 
