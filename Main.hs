#!/usr/bin/env cabal
{- cabal:
build-depends: base
            , arithmoi
            , modular-arithmetic
            , regex-compat
            , containers
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}


import Data.Char (ord, intToDigit, toUpper)
import Data.Maybe
import GHC.Bits
import Text.Regex
import Data.Word
import qualified Math.NumberTheory.Primes as P
import Math.NumberTheory.Primes hiding (precPrime, nextPrime, isPrime, primes)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (tails)
import Control.Monad

import Debug.Trace


import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))

windows :: Int -> [a] -> [[a]]
windows n0 = go 0 Seq.empty
  where
    go n s (a:as) | n' <  n0   =              go n' s'  as
                  | n' == n0   = toList s'  : go n' s'  as
                  | otherwise =  toList s'' : go n  s'' as
      where
        n'  = n + 1         -- O(1)
        s'  = s |> a        -- O(1)
        s'' = Seq.drop 1 s' -- O(1)
    go _ _ [] = []

isCoprime :: Integral a => a -> a -> Bool
isCoprime a b = gcd a b == 1

powMod :: (Integral a, Integral k) => a -> k -> a -> a
powMod _ 0 _ = 1
powMod a k n
  | even k = let x = powMod a (k `div` 2) n in (x * x) `mod` n
  | otherwise = (a * powMod a (k - 1) n) `mod` n

-- Test if nth root of a
isNthRoot :: (Integral a, Integral n) => a -> n -> Bool
isNthRoot a n = (floor ((fromIntegral a) ** (1 / fromIntegral n))) ^ n == a

isSquare :: Integral a => a -> Bool
isSquare = (`isNthRoot` 2)

isCube :: Integral a => a -> Bool
isCube = (`isNthRoot` 3)


factors :: (UniqueFactorisation a, Integral a) => a -> [a]
factors = factorisationToFactors . factorise
  where
    factorisationToFactors [] = [1]
    factorisationToFactors ((p, n) : fs) = do
      a <- map ((unPrime p)^) [0..n]
      b <- factorisationToFactors fs
      return $ a * b

properFactors :: (UniqueFactorisation a, Integral a) => a -> [a]
properFactors n = filter (\a -> a > 1 && a < n) $ factors n

precPrime :: (UniqueFactorisation a, Integral a, Bits a) => a -> a
precPrime = unPrime . P.precPrime

nextPrime :: (UniqueFactorisation a, Integral a, Bits a) => a -> a
nextPrime = unPrime . P.nextPrime

isPrime :: (UniqueFactorisation a, Integral a) => a -> Bool
isPrime = isJust . P.isPrime

primes :: (UniqueFactorisation a, Integral a) => [a]
primes = map unPrime P.primes

nthPowers :: Integral a => a -> [a]
nthPowers n = map (^n) [1..]

powersOfn :: Integral a => a -> [a]
powersOfn n = map (n^) [1..]

squares :: Integral a => [a]
squares = nthPowers 2

triangles :: Integral a => [a]
triangles = map (\n -> (n * (n + 1)) `div` 2) [1..]

pyramids :: Integral a => [a]
pyramids = map (\n -> (n * (n + 1) * (n + 2)) `div` 6) [1..]

range :: Integral a => a -> a -> [a] -> [a]
range a b = takeWhile (< b) . dropWhile (a >)

order :: (Integral a, Integral b) => a -> [b]
order i = [10^(i - 1) .. 10^i - 1]

withDigits :: Integral a => a -> [a] -> [a]
withDigits n = range (10^(n - 1)) (10^n)

matches :: Show a => String -> a -> Bool
matches pat = isJust . matchRegex (mkRegex pat) . show

withPattern :: Show a => String -> [a] -> [a]
withPattern pat = filter (matches pat)

multiples :: Num a => a -> [a]
multiples n = 0 : iterate (n *) n

isPalindrome :: Show a => a -> Bool
isPalindrome n = string == reverse string
  where string = show n

expand :: Integral a => a -> a -> [a]
expand _ 0 = []
expand b a = reverse $ go a b
  where
    go 0 _ = []
    go a b = (a `rem` b) : go (a `quot` b) b

digits :: Integral a => a -> [a]
digits = expand 10

unExpand :: Integral a => a -> [a] -> a
unExpand b = foldr (\d n -> n * b + d) 0

unDigits :: Integral a => [a] -> a
unDigits = unExpand 10

digitalRoot :: Integral a => a -> a
digitalRoot = until (< 10) (sum . digits)
  
multDigitalRoot :: Integral a => a -> a
multDigitalRoot  = until (< 10) (product . digits)


inBase :: Integral a => a -> a -> String
inBase b a
  | a < 0 || b < 2 = error "Both arguments must be non-negative, and base must be at least 2."
  | a == 0 = "0"
  | otherwise = map (toUpper . intToDigit . fromIntegral) $ expand b a
        
-- main :: IO ()
-- main = print . withPattern "1.1" . map fst . filter ((== 2) . uncurry (-) . swap). consecutivePairs . withDigits 3 $ (primes :: [Int])
--   where consecutivePairs = zip <*> tail
--         swap (x, y) = (y, x)


-- main :: IO ()
-- main = print . filter ( all (/= 0) . digits). map (2^) $ ([10..99]:: [Integer])
   
freqs :: Ord a => [a] -> Map a Int
freqs = foldr considerOccurence M.empty
  where
    considerOccurence = M.alter inc
    inc Nothing = Just 1
    inc (Just n) = Just $ n + 1

fibs :: Integral a => [a]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- main :: IO ()
-- main = print  . withPattern ".(0|1|2|4|6|9)1" . map fst . filter ((== 2) . uncurry (-) . swap). consecutivePairs . withDigits 3 $ (primes :: [Int])
--   where consecutivePairs = zip <*> tail
--         swap (x, y) = (y, x)



-- main :: IO ()
-- main = print . withPattern "1(1|3)39(1|3)9" . filter (all (== 2) . M.elems . freqs . digits) . withDigits 6 $ (primes :: [Int])


-- 34D  
-- main :: IO ()
-- main = print . filter (\n -> not (isPalindrome (inBase 2 n)) && isPalindrome (inBase 2 (n * n))) $ order 4

-- 3D
-- main :: IO ()
-- main = print . filter (\n -> (length . factors $ n) ^ 3 == n) . withDigits 5 $ nthPowers 3

-- 37D
-- main :: IO ()
-- main = print . withPattern ".8..." .  filter (all (\x -> 0 < x && x <=4). digits . (^2)) $ (order 5 :: [Int])


-- main :: IO ()
-- main = print . withPattern "...5." . withDigits 5 $ fibs


-- 22D
-- main :: IO ()
-- main = print . withPattern "..8" . withDigits 3 $ map (27*) [1..]

-- 29D
main :: IO ()
main = print . filter (\n -> (n `div` 2) `mod` 10 == 2). filter ((== 20) . sum . digits). withPattern "8.2.(1|2|3|4).4" . withDigits 7 $ map (7*) [1..]

-- 1D
-- main :: IO ()
-- main = print . withDigits 7  $ powersOfn 2

-- 41A
-- main :: IO ()
-- main = print . withPattern "...3.70(9|1|6|4|2|0)" . withDigits 8 $ map (719*) [1..]


-- 21A
-- main :: IO ()
-- main = print . withPattern ".04" . filter (\n -> not (isPrime n) && isCoprime 756 n) $ (order 3 :: [Int])

hasOrder :: Integral a => a -> a -> Bool
hasOrder n x = x >= 10^(n - 1) && x < (10^n - 1)

-- main :: IO ()
-- main = print . product . head . filter (hasOrder 6 . product). windows 4 . takeWhile (< 1000) $ fibs


-- facModn :: Integral a => a -> a -> a
-- facModn 0 n = 1
-- facModn a n = (a * facModn (a - 1) n) `mod` n

  
-- -- 23D
-- main :: IO ()
-- main = print . filter (\n -> (facModn (n - 1) (n^2) + 1) `mod` (n^2) == 0) $ order 3

-- main :: IO ()
-- main = print . withPattern "(4|1|8).1.(6|2).(6|7|8)" . filter (hasOrder 7) $ map (999*) [1001..10011]

-- 12A/4D
-- main :: IO ()
-- main = print . map (`div` 18) . withPattern "1.(0|5)." . map (18*) . filter odd $ ([57..99] :: [Int])


-- main :: IO ()
-- main = print $ do
--   b <- [7..16] :: [Int]
--   n <- [100..999]
--   guard $ inBase b n == "256"
--   return n


-- main :: IO ()
-- main = do
--   p <- primes
--   i <- [0..2]
--   r <- 
  
