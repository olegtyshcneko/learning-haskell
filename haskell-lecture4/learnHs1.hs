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

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999..])
                 where p x = x `mod` 3829 == 0
