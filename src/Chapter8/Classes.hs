module Chapter8.Classes where 
import Control.Concurrent

-- Classes define shared behaviour between types 

-- The following defines that to be an instance of the Eq class must support
-- Equality and inequality. 
class MyEq a where 
    (==), (/=) :: a -> a -> Bool 
    x /= y = not (x Chapter8.Classes.== y)

-- Since the class provides a default definition for inequality
-- the type only needs a definition for equality.

instance MyEq Bool where 
    False == False = True 
    True == True = True 
    _ == _ = False 

-- Only types delcared with data and newtypes can be made into instances of classes
-- Classes can also be extended to form new classes. Ord is a class whose types extend Eq

{-
class MyEq a => MyOrd a where 
    (<), (<=), (>), (>=) :: a -> a -> Bool
    min, max :: a -> a -> Bool 
    min x y 
        | x Chapter8.Classes.<= y = x 
        | otherwise = y 
    max x y 
        | x Chapter8.Classes.<= y = y
        | otherwise = x

-- The above means that to be an instance of Ord, the type must be and instance of Eq
-- And support all the other operations of the Ord class
-}