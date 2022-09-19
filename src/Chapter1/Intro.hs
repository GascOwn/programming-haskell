module Chapter1.Intro where

-- A quicksort using list comprehensions and recursion
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where 
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

-- Sequencing actions with moands

seqn :: Monad m => [m a] -> m [a]
seqn [] = return []
seqn (act:acts) = do 
    x <- act
    xs <- seqn acts 
    return (x:xs)

