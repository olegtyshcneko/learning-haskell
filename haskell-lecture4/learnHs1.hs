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
              
numLongChainsLambda = length (filter (\xs -> length xs > 15) (map chain [1..100]))

flipLambda :: (a -> b -> c) -> b -> a ->c
flipLambda f = \x y -> f y x

sumWithFold :: (Num a) => [a] -> a
sumWithFold xs = foldl (\acc x -> acc + x) 0 xs

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

elemWithFold :: (Eq a) => a -> [a] -> Bool
elemWithFold y = foldl (\acc x -> if x==y then True else acc) False

mapWithFold :: (a -> b) -> [a] -> [b]
mapWithFold f xs = foldr (\x acc -> f x : acc) [] xs

maximumWithFold :: (Ord a) => [a] -> a
maximumWithFold = foldr1 (\x acc -> if x > acc then x else acc)

reverseWithFold :: [a] -> [a]
reverseWithFold = foldl (flip (:)) []

productWithFold :: (Num a) => [a] -> a
productWithFold = foldr1 (*)

filterWithFold :: (a -> Bool) -> [a] -> [a]
filterWithFold p = foldr (\x acc -> if p x then x : acc else acc) []

headWithFold :: [a] -> a
headWithFold = foldr1 (\x _ -> x)

lastWithFold :: [a] -> a
lastWithFold = foldl1 (\_ x -> x)
