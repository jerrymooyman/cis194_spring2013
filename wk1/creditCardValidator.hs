module CreditCardValidator where

-- Exercise 1
toDigitList :: Integer -> [Integer]
toDigitList 0 = []
toDigitList n
  | n < 0 = []
  | otherwise = toDigitList (n `div` 10) ++ [n `mod` 10]

toDigitListRev :: Integer -> [Integer]
toDigitListRev n =  reverseList (toDigitList n)

reverseList :: [Integer] -> [Integer]
reverseList []     = []
reverseList (x:xs) = reverseList xs ++ [x]

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []      = []
doubleEveryOther [x]     = [x]
doubleEveryOther (x:y:z) = [x] ++ [y*2] ++ doubleEveryOther z

-- Exercise 3
reduceDigit :: Integer -> Integer
reduceDigit n
  | n `div` 10 /= 0 = (n `div` 10) + (n `mod` 10)
  | otherwise = n

sumListDigits :: [Integer] -> Integer
sumListDigits = sum . map reduceDigit

-- Exercise 4
validate :: Integer -> Bool
validate n = (sumListDigits . doubleEveryOther . reverseList . toDigitList) n `mod` 10 == 0
