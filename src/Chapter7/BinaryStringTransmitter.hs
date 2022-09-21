{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list literal" #-}
{-# HLINT ignore "Use list literal pattern" #-}
module Chapter7.BinaryStringTransmitter where

import Data.Char

type Bit = Int

bin2intComp :: [Bit] -> Int
bin2intComp bits = sum [w * b | (w, b) <- zip weights bits]
    where weights = iterate (*2) 1

-- iterate f x = [x, f x, f (f x), f (f (f x)), ...] so it's [1, 1 * 2, (1 * 2) * 2, etc]

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y ) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

setParityBit :: [Bit] -> [Bit]
setParityBit [] = []
setParityBit list
    | even . length . filter (==1) $ list = 0 : list
    | otherwise = 1 : list

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concatMap (setParityBit . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

checkParityBit :: [Bit] -> [Bit]
checkParityBit [] = error "Empty binary"
checkParityBit (x:xs)
    | (even . length . filter (==1) $ xs) && x == 0 = xs
    | (odd . length . filter (==1) $ xs) && x == 1 = xs
    | otherwise = error "Error in binary transimission"


decode :: [Bit] -> String
decode = map ((chr . bin2int) . checkParityBit) . chop9

transmit ::  String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id