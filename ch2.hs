import Data.String (IsString)
import Distribution.SPDX (LicenseId (AFL_2_0, AFL_3_0))

-- This file is referred to as "prime.hs" in the text.
-- You can run it with

-- $ ghci
-- *Main> :load ch2

t = True

f = False

-- Exercise 2.2
-- Make a truth table for exclusive or (a.k.a "either or")
-- P Q | P V Q
-- T T |   F
-- T F |   T
-- F T |   T
-- F F |   F

-- iff
x <=> y = x == y

test_iff = do
  print (t <=> t && f <=> f)

-- Example 2.3

-- A convenience func for calling other functions
main = do
  test_iff