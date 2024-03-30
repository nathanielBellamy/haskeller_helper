module Primes.Primes (
  primes
) where

import Data.List
import Data.Bool

primes :: String -> IO()
primes x = let i = read x :: Int
  in do
    putStrLn ("We will find primes up to: " ++ x)
    putStrLn (show (findPrimes i))

findPrimes :: Int -> [Int]
findPrimes lim
  | lim < 2       = []
  | otherwise     = exec 3 lim [2]

exec :: Int -> Int -> [Int] -> [Int]
exec curr lim xs
  | curr >= lim              = xs
  | divisibleByElem curr xs  = exec (curr + 1) lim xs
  | otherwise                = exec (curr + 1) lim (curr : xs)

divisibleByElem :: Int -> [Int] -> Bool
divisibleByElem _ []      = False
divisibleByElem 0 _       = True
divisibleByElem n (x:xs)
 | rem n x == 0           = True
 | otherwise              = divisibleByElem n xs


