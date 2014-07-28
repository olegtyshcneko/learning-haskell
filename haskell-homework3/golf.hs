module Golf where

takeEveryN :: Int -> [a] -> [a]
takeEveryN n [] = []
takeEveryN n xs
           | n > length xs = []
           | otherwise = last (take n xs) : takeEveryN n (drop n xs)

skips :: [a] -> [[a]]
skips [] = []
skips x = takeFromOneToLen 1 (length x) []
      where takeFromOneToLen n len xs
                             | n > len = reverse xs
                             | otherwise = takeFromOneToLen (n+1) len ((takeEveryN n x) : xs)

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima xs 
            | length xs < 3 = []
            | otherwise = if maxima firstThree
                             then head (drop 1 firstThree) : localMaxima (drop 2 xs)
                             else localMaxima (drop 1 xs)
              where maxima (x1:x2:x3:xs) = x2 > x1 && x2 > x3
                    firstThree = take 3 xs
