{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use <$>" #-}
module Chapter12.StateMonad where

{- 
    There is the problem of writing functions that manipulate some form of state
    that can be changed over time. Let's assume, for simplicity, that the state
    is a simple Int, but it can be modified as required.

    The most basic form of function on this type is the state transformer (ST),
    which takes an input state as its argument an produces and output state which
    reflects any updates that were made to the state by the function during its
    execution.

    Since we also may wish to return a result value in addition to updating the
    state. For example, if the state represents a counterm a function for
    incrementing the counter may also wish to return its current value.

    A state transformer may also wish to take argument values, and also be made
    into a monad so that the do notation can be used to write stateful programs.
    Since types declared using the type Mechanism cannot be made into instances
    of classes, we must define State using newtype, which requires a dummy 
    constructor State
-}

newtype State a = State (Int -> (a, Int))

-- It's useful to define a function that removes the dummy constructor 
app :: State a -> Int -> (a, Int)
app (State st) x = st x

-- The first step of making ST into a Monad is defining a Functor instance

instance Functor State where 
    fmap g st = State (\s -> let (x, s') = app st s in (g x, s'))

{-
    That is, fmap allows us to apply a function to the result value of a state 
    transformer. The let mechanism in a function is similar to where, but the 
    local definition is made at the level of expressions rather than function
    definition.

    The type ST can also be made into an Applicative functor:
-}

instance Applicative State where
    pure x = State (\s -> (x,s))
    stf <*> stx = State (\s -> 
        let (f,s') = app stf s 
            (x,s'') = app stx s' 
        in (f x, s''))

{-
    In this case, pure transforms a value into a state tranformer that returns 
    the value without modifying the state

    The <*> operator applies a state transformer that returns a function to a 
    state transformer that returns an argument to give a state transformer that
    returns the result of applying the function to the argument.

    x <*> y just makes the ST action that runs x to get a function,
    then runs y to get a value, then applies the function to the value and 
    returns that.

    The monadic instance of State can be declared as 
-}

instance Monad State where
    st >>= f = State (\s -> let (x,s') = app st s in app (f x) s')

{-   
    st >>= f applies the state transformer st to an initial state s, then applies
    function f to the resulting value x to give a new state transformer f x,
    which is applied to the new state s' to give the final result.

    The bind operator integrates the sequencing of state transformers with the
    processing of their result values. 
-}

-- Example with Trees 

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

{-
    As an example, we relabel every leaf of the above tree with integers.
    This can be done by taking the next integer as an additional argument,
    and returning it as an additional result:
-}

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n + 1)
rlabel (Node l r) n = (Node l' r', n'')
    where
    (l', n') = rlabel l n
    (r', n'') = rlabel r n'

{-
    Doing so recursively gives a complicated definition that needs to keep track
    of the integer state through the computation. Tree a -> Int -> (Tree Int, Int) 
    can be rewritten as Tree a -> State (Tree Int), so the State can be generated
    by defining a state transformer that returns the current state as its result
    and the next integer as the new state:
-}

fresh :: State Int
fresh = State (\n -> (n, n + 1))

{-
    Since state in an Applicative, a new version of the function can be written
    in applicative style:
-}

alabel :: Tree a -> State (Tree Int)
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

{-
    The base case applies the Leaf constructor to the fresh integer, while the
    recursive case applies the Node constructor to the result of labelling the two
    substrees. This allows to automate the threading of the integer state through
    the computation.

    State is also a Monad, so an equivalent monadic function can be written:
-}

mlabel :: Tree a -> State (Tree Int)
mlabel (Leaf _) = do
    n <- fresh
    return (Leaf n)
mlabel (Node l r) = do
    left <- mlabel l 
    right <- mlabel r
    return (Node left right) 


{- 
    State is now defined like this: 

    newtype State s a = State { runState :: s -> (a, s)}

    and its constructor function:

    state :: (s -> (a, s)) -> State s a
    state = State
-}

