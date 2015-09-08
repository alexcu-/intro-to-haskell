{-  Homework 1 Exercises from Intro To Haskell Course
    http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf -}

module Homework1 where

-- Exercise 1: Number to array of digits

splitDigits :: Integer -> [Integer] -> [Integer]
splitDigits n prev
  | lef > 0   = splitDigits lef (rem : prev)
  | otherwise = rem : prev
  where rem = n `mod` 10
        lef = n `div` 10

toDigitsRev :: Integer -> [Integer]
toDigitsRev n =
  reverse (toDigits n)

toDigits :: Integer -> [Integer]
toDigits n =
  if n <= 0 then [] else splitDigits n []

ex1pass =
  toDigits 1234 == [1,2,3,4] &&
  toDigitsRev 1234 == [4,3,2,1] &&
  toDigits 0 == [] &&
  toDigits (-17) == []

-- Exercise 2: Double ESO from RTL

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n =
  let nums = zip [1..(length n)] (reverse n) in
    [if i `mod` 2 == 0 then x * 2 else x | (i, x) <- reverse nums]

ex2pass =
  doubleEveryOther [8,7,6,5] == [16,7,12,5] &&
  doubleEveryOther [1,2,3] == [1,4,3]

-- Exercise 3: Sum of Digits

sumDigits :: [Integer] -> Integer
sumDigits n =
  foldr (+) 0 (concat [splitDigits x [] | x <- n])

ex3pass =
  sumDigits [16,7,12,5] == 22

-- Exercise 4: Validate
-- 1. Double ESO using doubleEveryOther
-- 2. Add the digits of (1) using sumDigits
-- 3. If (2) `mod` 10 == 0, then valid

validate :: Integer -> Bool
validate n =
  sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

ex4pass =
  validate 4012888888881881 == True &&
  validate 4012888888881882 == False

main = print (ex4pass)