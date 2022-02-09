import Distribution.SPDX (LicenseId(AFL_3_0))
import Data.String (IsString)
-- This file is referred to as "prime.hs" in the text.

-- Exercise 1.3
divides d n = rem n d == 0

-- We can partially apply ldf to make ld
ld = ldf 2


-- A "guarded" equation
ldf k n | divides k n = k
        | k^2 > n     = n
        | otherwise   = ldf (k + 1) n
-- Note that ldf 3 8 will, counter-intuitively, return 8.


-- Exercise 1.4
-- Changing from k^2 > n to >= n
ldf2 k n | divides k n = k
         | k^2 >= n    = n
         | otherwise   = ldf (k + 1) n
-- This doesn't make any difference because if k^2 == n then k divides n


prime0 n | n < 1 = error "not a positive integer"
         | n == 1 = False
         | otherwise = ld n == n

-- Exercise 1.5
-- Quick visual check of prime0
ex1_5 = do
    print (prime0 1)
    print (prime0 2)
    print (prime0 3)
    print (prime0 4)


-- Exercise 1.6
-- The type def for rem (remainder) would be 
-- rem :: Integral t => t -> t -> t


-- Exercise 1.7
-- Note that the type declaration can be all the way down here
divides :: Integer -> Integer -> Bool


-- Exercise 1.9
-- Define a max' function using the predefined max
max' [] = error "max' function cannot work on an empty list"
max' [x] = x
max' (x:xs) = max x (max' xs)


-- Exercise 1.10
-- Define removeFst, that removes the first occurrence of m from a list of Ints
removeFst :: Int -> [Int] -> [Int]
removeFst m [] = []
removeFst m (x:xs) | m == x = xs
                   | otherwise = x : removeFst m xs

test_removeFst = do
    print(removeFst 3 [2, 3, 4] == [2, 4])
    print(removeFst 2 [200] == [200])


-- Example 1.11
mnmInt :: [Int] -> Int
mnmInt [] = error "empty list"
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : srtInts (removeFst m xs) where m = mnmInt xs

test_srtInts = do
    print(srtInts [5, 4, 3, 2])


-- Example 1.12
-- Note fromInt seems to now be fromIntegral
average :: [Int] -> Float
average [] = error "empty list"
average xs = fromIntegral (sum' xs) / fromIntegral (len' xs)

sum' :: [Int] -> Int
sum' xs = foldr (+) 0 xs

len' :: [a] -> Int
len' xs = foldr (\ x -> (+) 1) 0 xs


-- Exercise 1.13
-- A function to count the number of occurrences of a char in a string
countChar :: Char -> String -> Int
countChar c [] = 0
countChar c (x:xs) | x == c = 1 + countChar c xs
                   | otherwise = countChar c xs

test_countChar = do
    print(countChar 'c' "ccc" == 3)
    print(countChar 'd' "eee" == 0)
    print(countChar 'g' "hgh" == 1)


-- Exercise 1.14

-- Partially apply subBlowup to make blowup
blowup :: String -> String
blowup = subBlowup 1

subBlowup :: Int -> String -> String
subBlowup n "" = ""
subBlowup n (x:xs) = repeatChar n x ++ subBlowup (n + 1) xs

repeatChar :: Int -> Char -> String
repeatChar n c | n < 1 = error "n must be >= 1"
               | n == 1 = [c]
               | otherwise = c : repeatChar (n-1) c

test_blowup = do
    print(blowup "bang!" == "baannngggg!!!!!")


-- Exercise 1.15
-- Write srtString to sort strings in alphabetical order

-- We write a general version of removeFst from the earlier example
removeFstG :: Eq a => a -> [a] -> [a]
removeFstG m [] = []
removeFstG m (x:xs) | m == x = xs
                    | otherwise = x : removeFstG m xs

-- We write a general version of mnmInt from the earlier example
mnmG :: Ord a => [a] -> a
mnmG [] = error "empty list"
mnmG [x] = x
mnmG (x:xs) = min x (mnmG xs)

-- Strings can now be sorted the same way integers were
srtStr :: String -> String
srtStr "" = ""
srtStr xs = m : srtStr (removeFstG m xs) where m = mnmG xs

test_srtString = do
    print(srtStr "xy" == "xy")
    print(srtStr "yx" == "xy")
    print(srtStr "yxz" == "xyz")

-- Example 1.16
prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x : xs) [] = False
prefix (x : xs) (y : ys) = (x == y) && prefix xs ys

-- Exercise 1.17
-- Write a function that checks whether str1 is a substring of str2
substring :: String -> String -> Bool
substring str1 [] = False
substring str1 (x:xs) = prefix str1 (x:xs) || prefix str1 xs


-- A convenience func for calling other functions
main = do
    ex1_5
    print ""
    test_removeFst
    test_srtInts
    test_countChar
    test_blowup
    test_srtString
