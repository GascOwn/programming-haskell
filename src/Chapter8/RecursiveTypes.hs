module Chapter8.RecursiveTypes where

-- Recursive types are data declarations of types defined in terms of themselves

data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat 
int2nat 0 = Zero 
int2nat n = Succ (int2nat (n-1))

addNat :: Nat -> Nat -> Nat
addNat Zero n = n
addNat (Succ m) n = Succ (addNat m n)


-- The data mechanism can be used to define data structures such as lists
data List a = Nil | Cons a (List a)

len :: List a -> Int 
len Nil = 0 
len (Cons _ xs) = 1 + len xs

-- Or trees

data Tree a = Leaf a | Node (Tree a) a (Tree a)

exTree :: Tree Int 
exTree = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r  

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

occursSorted :: Ord a => a -> Tree a -> Bool 
occursSorted x (Leaf y) = x == y
occursSorted x (Node l y r) 
    | x == y = True 
    | x < y = occursSorted x l
    | otherwise = occursSorted x r