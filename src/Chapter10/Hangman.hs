module Chapter10.Hangman where

import System.IO(hSetEcho, stdin)

hangman :: IO ()
hangman = do
    putStrLn "Think of a word:"
    word <- sgetLine
    putStrLn "Try to guess it:"
    play word

sgetLine :: IO String 
sgetLine = do 
    x <- getCh
    case x of 
        '\n' -> do
            putChar x
            return []
        _ -> do 
            putChar '-'
            xs <- sgetLine
            return (x:xs)

getCh :: IO Char 
getCh = do
    hSetEcho stdin False 
    x <- getChar
    hSetEcho stdin True
    return x 

match :: String -> String -> String
match xs ys = [if x `elem` ys then x else '-' | x <- xs]

play :: String -> IO ()
play word = do 
    putStr "? "
    guess <- getLine 
    if guess == word  then putStrLn "You got it!!"
    else do
        putStrLn (match word guess)
        play word 

