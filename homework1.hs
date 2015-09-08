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

-- Exercise 5: Towers of Hanoi
-- 1. move n − 1 discs from a to c using b as temporary storage
-- 2. move the top disc from a to b
-- 3. move n − 1 discs from c to b using a as temporary storage.

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
-- Terminator Recursive Clause | n = 1
-- When moving one disk, top disk goes from a to b (that is the move)
hanoi 1 a b c = [(a, b)]
-- When moving n-disks, apply rules for list of each move needed
hanoi n a b c =
  hanoi (n-1) a c b ++ -- Move n - 1 disks from a to c using b as tmp storage
  hanoi 1 a b c     ++ -- Move top disk from a to b, c is static
  hanoi (n-1) c b a    -- Move n - 1 disks from c to b using a as tmp storage

ex5pass =
  hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]

main = print (ex5pass)