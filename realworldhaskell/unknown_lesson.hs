add :: Integer -> Integer -> Integer
add a b = a + b

myDrop :: Int -> [a] -> [a]
myDrop n xs = if n <= 0 || null xs
                 then xs
                 else myDrop (n-1) (tail xs)

lastButOne :: [a] -> a
lastButOne xs = head (drop (length xs - 2) xs)

lastButOneInit :: [a] -> a
lastButOneInit xs = last (init xs)

lastButOneRec :: [a] -> a
lastButOneRec xs = if (length xs) == 2
                      then head xs
                      else lastButOneRec (tail xs)


toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n = (n `mod` 10) : toDigitsRev (n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:zs) = x : y*2 : doubleEveryOther zs

sumDigit :: Integer -> Integer
sumDigit n = sum (toDigits n)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sumDigit x) + sumDigits xs

validate :: Integer -> Bool
validate n = doubledSum `mod` 10 == 0
 where
  digits = toDigitsRev n
  doubledDigits = doubleEveryOther digits
  doubledSum = sumDigits doubledDigits


--hanoi algorithm
--todo: think about it at home and try it's algo on others languages? (clojure)
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 beg mid end = [(beg, end)]
hanoi n beg mid end = (hanoi (n-1) beg end mid) ++ (hanoi 1 beg mid end) ++ (hanoi (n-1) mid beg end)
