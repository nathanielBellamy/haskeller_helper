module Src.Primes.Primes (
  primes
) where

import Data.List
import Data.Bool

primes :: String -> IO()
primes x =
  let i   = read x :: Int
      res = primesUpTo i
  in do
    putStrLn ("We will find primes up to: " ++ x)
    putStrLn (show res)

primesUpTo :: Int -> [Int]
primesUpTo lim
  | lim < 2       = []
  | otherwise     = findPrimes 3 lim [2]

findPrimes :: Int -> Int -> [Int] -> [Int]
findPrimes curr lim ps
  | curr >= lim              = ps
  | divisibleByElem curr ps  = findPrimes (curr + 1) lim ps
  | otherwise                = findPrimes (curr + 1) lim (curr : ps)

divisibleByElem :: Int -> [Int] -> Bool
divisibleByElem _ []      = False
divisibleByElem 0 _       = True
divisibleByElem n (x:xs)
 | rem n x == 0           = True
 | otherwise              = divisibleByElem n xs

