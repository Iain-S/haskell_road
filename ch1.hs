-- Referred to as "prime.hs" in the text.

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
-- Define removeFst, that removes the first occurrence from a list of Ints
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

-- A convenience func for calling other functions
main = do
    ex1_5
    print ""
    test_removeFst
    test_srtInts
