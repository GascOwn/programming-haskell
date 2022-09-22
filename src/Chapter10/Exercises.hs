module Chapter10.Exercises where

import Chapter10.Nim(Board, putRow)

putStrComp :: String -> IO ()
putStrComp a = sequence_ [return x | x <- a, x /= '\n']

putBoard' :: Int -> Board -> IO ()
putBoard' _ [] = return ()
putBoard' r (n:ns) = do 
    putRow r n
    putBoard' (r + 1) ns 

putBoardRec :: Board -> IO ()
putBoardRec = putBoard' 1  

putBoardComp :: Board -> IO ()
putBoardComp a = sequence_ [putRow x y| x <- [1..], y <- a]
