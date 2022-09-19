module Chapter2.Functions where
import Chapter1.Exercises

takeDouble :: [Integer]
takeDouble = take (double 2) [1,2,3,4,5]

factorial :: (Num a, Enum a) => a -> a
factorial n = product [1..n]

average :: Foldable t => t Int -> Int
average ns = div (sum ns) (length ns)

exerciseFunc :: Int
exerciseFunc = div a (length xs)
    where
        a = 10
        xs = [1,2,3,4,5]

myLast :: [a] -> a
myLast list = list !! (length list - 1)

myInit :: [a] -> [a]
myInit list = take (length list - 1) list

