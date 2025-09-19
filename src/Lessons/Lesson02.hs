module Lessons.Lesson02 (f,f',trd, FireExtinguisher(..), FEType(..),
    capacity, refill, length', length'') where

-- | Return the head of a list, or 0 if the list is empty
--
-- * @f [] == 0@
-- * @f (h:_) == h@
--
-- Underscore matches any element but ignores it
f :: [Integer] -> Integer
f [] = 0 
f (h:_) = h

-- | Must provide two elements, if only one is provided throws exception
--
-- * @f' (h1:h2:_) = h1@
-- * @f' [] = 0@
f' :: [Integer] -> Integer
f' (h1:h2:_) = h1
f' [] = 0

-- | Tuple
--
-- * @t1 = (42, 'a')@
t1 :: (Integer, Char)
t1 = (42, 'a')

-- * @t2 = (43, -1, "labas")@
t2 :: (Integer, Int, String)
t2 = (43, -1, "labas")

-- | a, b, c - undefined types
trd :: (a, b, c) -> c
trd (_, _, v) = v

-- f'' :: [a] -> a
-- f'' [] = 0
-- f'' (h:_) = h


-- ADT - Algerbraic data type
data FEType = A | B | C deriving Show
data FireExtinguisher = FireExtinguisher Integer FEType deriving Show

capacity :: FireExtinguisher -> Integer
capacity (FireExtinguisher c _) = c

refill :: FireExtinguisher -> FireExtinguisher
refill (FireExtinguisher _ t) = FireExtinguisher 10 t

-- | Second if approves that there is at least one element
--    
-- Function's last call is not recursion itself 
--
-- * @length' [] = 0@
-- * @length' (_:t) = 1 + length' t@
length' :: [a] -> Int
length' [] = 0
length' (_:t) = 1 + length' t

-- | @'@Where@'@ makes function local
--
-- Acc - accumulator
--
-- Tail recursive call can be used when recursion is last operation
length'' :: [a] -> Int
length'' l = length''' l 0
    where
        length''' :: [a] -> Int -> Int
        length''' [] acc = acc
        length''' (_:t) acc = length''' t (acc + 1)