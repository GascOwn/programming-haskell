module Main where

import Chapter7.Folds

main :: IO ()
main = print $ foldReverse [1,2,3,4]
