module Chapter7.HigherOrder where 

-- In Haskell you can have functions that take functions as arguments

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- Being a curried function, it can be partially applied to create new functions.

backToBasics :: [a] -> [a]
backToBasics = twice reverse 

sameAsBefore :: [Integer]
sameAsBefore = backToBasics [1,2,3] 