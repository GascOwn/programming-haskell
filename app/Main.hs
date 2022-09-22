module Main where

import Chapter7.Folds
import Chapter12.Functors

main :: IO ()
main = do 
    print $ foldReverse [1,2,3,4]
    print $ areNodesEven (Node (Leaf 1) 2 (Leaf 3))

