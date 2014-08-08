zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
      where g x y = f y x

flip2 :: (a -> b -> c) -> b -> a -> c
flip2 f y x = f x y

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' pred (x:xs)
        | pred x = x : filter' pred xs
        | otherwise = filter' pred xs

--example of how easy we can do calculations in haskell. filter will run while predicate is not true.
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999..])
                 where p x = x `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n 
      | even n = n : chain (n `div` 2)
      | odd n  = n : chain (n*3 + 1)
      
numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
              where isLong xs = length xs > 15
