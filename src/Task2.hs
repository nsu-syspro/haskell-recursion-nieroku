{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- The above pragma enables all warnings
-- (except for unused imports from Task1)

module Task2 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment

-- You can reuse already implemented functions from Task1
-- by listing them in this import clause
-- NOTE: only listed functions are imported, everything else remains hidden

import Data.Char (ord)
import Task1 (doubleEveryOther, map, reverse, sum, toDigits)
import Prelude hiding (filter, foldl, foldr, head, init, last, length, map, read, reverse, show, sum, tail)

-----------------------------------
--
-- Computes check digit number for given abstract characters using Luhn algorithm mod N
-- and given mapping function
--
-- Usage example:
--
-- >>> luhnModN 10 id [3,4,5,6]
-- 1

luhnModN :: Int -> (a -> Int) -> [a] -> Int
luhnModN n charToDigit chars = (n - (luhnSum `rem` n)) `rem` n
  where
    digits = map charToDigit chars
    luhnSum = (sum . (map normalize) . doubleEveryOther . reverse) digits
    normalize k
      | k >= n = k - (n - 1)
      | otherwise = k

-----------------------------------
--
-- Computes decimal check digit for given digits using Luhn algorithm mod 10
--
-- Usage example:
--
-- >>> luhnDec [3,4,5,6]
-- 1

luhnDec :: [Int] -> Int
luhnDec = luhnModN 10 id

-----------------------------------
--
-- Computes hexadecimal check digit number for given digits using Luhn algorithm mod 16
--
-- Usage example:
--
-- >>> luhnHex "123abc"
-- 15

luhnHex :: [Char] -> Int
luhnHex = luhnModN 16 digitToInt

-----------------------------------
--
-- Converts given hexadecimal digit to its ordinal number between 0 and 15
--
-- Usage example:
--
-- >>> map digitToInt ['0'..'9']
-- [0,1,2,3,4,5,6,7,8,9]
-- >>> map digitToInt ['a'..'f']
-- [10,11,12,13,14,15]
-- >>> map digitToInt ['A'..'F']
-- [10,11,12,13,14,15]

digitToInt :: Char -> Int
digitToInt digit
  | '0' <= digit && digit <= '9' = ord digit - ord '0'
  | 'a' <= digit && digit <= 'f' = 10 + ord digit - ord 'a'
  | 'A' <= digit && digit <= 'F' = 10 + ord digit - ord 'A'
  | otherwise = undefined

-----------------------------------
--
-- Checks whether the last decimal digit is a valid check digit
-- for the rest of the given number using Luhn algorithm mod 10
--
-- Usage example:
--
-- >>> validateDec 3456
-- False
-- >>> validateDec 34561
-- True
-- >>> validateDec 34562
-- False

validateDec :: Integer -> Bool
validateDec n = luhnDec digits == checkDigit
  where
    checkDigit = fromInteger $ n `rem` 10
    digits = toDigits $ n `div` 10

-----------------------------------
--
-- Checks whether the last hexadecimal digit is a valid check digit
-- for the rest of the given number using Luhn algorithm mod 16
--
-- Usage example:
--
-- >>> validateHex "123abc"
-- False
-- >>> validateHex "123abcf"
-- True
-- >>> validateHex "123abc0"
-- False

validateHex :: [Char] -> Bool
validateHex digits = luhnHex (init digits) == checkDigit
  where
    checkDigit = digitToInt $ last digits

init :: [a] -> [a]
init [] = undefined
init [_] = []
init (x : xs) = x : init xs

last :: [a] -> a
last [] = undefined
last [x] = x
last (_ : xs) = last xs
