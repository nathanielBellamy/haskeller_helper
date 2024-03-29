module Primes.Primes (
  primes
) where

primes :: String -> IO()
primes x = do
  -- X = read x :: Integer
  putStrLn ("We will find primes up to: " ++ x)

