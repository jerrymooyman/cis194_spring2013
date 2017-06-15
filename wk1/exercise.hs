toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n
  | n < 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10] 

toDigitsRev :: Integer -> [Integer]
toDigitsRev n =  reverseList (toDigits n)

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:z) = [x] ++ [(y*2)] ++ doubleEveryOther z



-- reverseList1 :: [a] -> [a]
-- reverseList1 l = _reverseList l []
--   where
--     _reverseList :: [a] -> [a] -> [a]
--     _reverseList [] l = l
--     _reverseList (x:xs) l = _reverseList xs (x:l)

-- doubleEveryOther :: [Integer] -> [Integer]
-- doubleEveryOther l = _doubleEveryOther l []
--   where 
--     _doubleEveryOther :: [Integer] -> [Integer] -> [Integer]
--     _doubleEveryOther [] l = l
--     _doubleEveryOther (x:y:z) l = x:(y*2):_doubleEveryOther z 
