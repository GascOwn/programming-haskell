{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Chapter10.InteractiveProgramming where 

-- An interactive program in one that performs side effects 
-- eg reading a character from keyboard and returning it
-- Interactive programs return a result value wrapped in the IO type -> IO Int
-- Expressions of type IO are called actions

-- In Haskell, a sequence of IO actions can be combined in a do block

printAChar :: IO ()
printAChar = do
    input <- getChar -- getChar gets from standard input
    putChar input -- putChar prints to standard output


-- This action reads three characters and returns the first and third as a pair
act :: IO (Char, Char)
act = do 
    x <- getChar
    getChar
    y <- getChar
    return (x,y)

-- This can be done recursively 

getMyLine :: IO [Char]
getMyLine = do
    x <- getChar 
    case x of 
        '\n' -> return []
        _ -> do 
            xs <- getMyLine
            return (x:xs)

putMyStr :: String -> IO()
putMyStr [] = return ()
putMyStr (x:xs) = do 
    putChar x
    putMyStr xs

putMyStrLn :: String -> IO ()
putMyStrLn xs = do 
    putMyStr xs
    putChar '\n'

-- Using the above primitives we can write an action that calculates the length
-- of a string taken from standard input
strlen :: IO ()
strlen = do 
    putMyStr "Enter a string: "
    xs <- getMyLine
    putMyStr "The string has "
    putMyStr . show . length $ xs
    putMyStrLn " characters"

