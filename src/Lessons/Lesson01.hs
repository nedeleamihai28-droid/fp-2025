-- | Module is a unit of compilation.

module Lessons.Lesson01 (i, ii, c, s, b, f, add, il, cl) where


-- >>> i + i == 84
-- True
--- >>> 5
--- 5

-- | Endless
i :: Integer    
i = 42

-- | An Int constant (value, not variable, it does not vary)
ii :: Int 
ii = 43

c :: Char
c = 'a'

s :: String
s = "labas"

b :: Bool
b = True

-- | Checks if the given age is at least 20
--
-- * @f age = age >= 20@
f :: Integer -> Bool
f age = age >= 20

-- | Adds two integers
--
-- * @add a b = a + b@
add :: Integer -> Integer -> Integer
add a b = a + b

il :: [Integer]
il = [42, 42, 42]

cl :: [Char]
cl = "labas"
