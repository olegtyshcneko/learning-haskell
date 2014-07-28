import Data.List

data BookInfo = Book Int String [String]
 deriving Show
 
data MagazineInfo = Magazine Int String [String]
 deriving Show
 

myInfo :: BookInfo
myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]

type CustomerId = Int
type ReviewBody = String

data BookReview = BookReview BookInfo CustomerId ReviewBody

type BookRecord = (BookInfo, BookReview)

type CardHolder = String
type CardNumber = String
type Adress = [String]

data BillingInfo = CreditCard CardNumber CardHolder Adress
                   | CashOnDelivery
                   | Invoice CustomerId
                   deriving Show
                   
data Cartesian2D = Cartesian2D Double Double
                   deriving (Eq, Show)
                   
data Polar2D = Polar2D Double Double
               deriving (Eq, Show)
               
myNot True = False
myNot False = True

getBookId :: BookInfo -> Int
getBookId (Book id _ _) = id

getBookTitle :: BookInfo -> String
getBookTitle (Book _ title _) = title

getBookAuthors :: BookInfo -> [String]
getBookAuthors (Book _ _ authors) = authors

--forgot to pattern match all elements
badExample (x:xs) = x + badExample xs


--records (kill boilerPlate with accessors)
data Customer = Customer {
     customerId :: CustomerId,
     customerName :: String,
     customerAdress :: Adress
} deriving Show


data MyList a = Cons a (MyList a)
              | Nil
                deriving Show
              
fromList :: [a] -> MyList a
fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

toList :: MyList a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

data MaybeTree a = MNode a (Maybe (Tree a)) (Maybe (Tree a))
 deriving Show


--using error to abort evalution and give exception
mySecond :: [a] -> a
mySecond xs = if null (tail xs)
                 then error "list is too short"
                 else head (tail xs)
                
--using Maybe is better
mySecond' :: [a] -> Maybe a
mySecond' [] = Nothing
mySecond' xs = if null (tail xs)
                  then Nothing
                  else Just (head (tail xs))
                  
tidySecond :: [a] -> Maybe a
tidySecond (_:x:_) = Just x
tidySecond _ = Nothing

lend :: Integer -> Integer -> Maybe Integer
lend amount balance = let  reserve = 100
                           newBalance = balance - amount
                      in if balance < reserve
                            then Nothing
                            else Just newBalance
                            
lend2 amount balance = if amount < reserve
                          then Just newBalance
                          else Nothing
  where reserve = 100
        newBalance = balance - amount

pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
 where plural 0 = "no " ++ word ++ "s"
       plural 1 = "one" ++ word
       plural n = show n ++ " " ++ word ++ "s"

--using guards:
nodesAreSame :: Eq a => Tree a -> Tree a -> Maybe a
nodesAreSame (Node a _ _) (Node b _ _)
             | a == b = Just a
nodesAreSame _ _ = Nothing

--using guards and where clause
lend3 :: (Fractional a, Ord a) => a -> a -> Maybe a
lend3 amount balance
      | amount <= 0 = Nothing
      | amount > reserve * 0.5 = Nothing
      | otherwise = Just newBalance
      where reserve = 100
            newBalance = balance - amount

--mydrop using guards
niceMyDrop :: Int -> [a] -> [a]
niceMyDrop n xs | n <= 0 = xs
niceMyDrop _ [] = []
niceMyDrop n (_:xs) = niceMyDrop (n-1) xs

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs
 
listMean ::  (Fractional a, Integral b) => [b] -> a
listMean xs = (fromIntegral (sum xs)) / (fromIntegral (length xs))

makePalindrome :: [a] -> [a]
makePalindrome xs = xs ++ reverse xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

compareLength :: [a] -> [a] -> Ordering
compareLength x y
              | (length x) < (length y) = LT
              | otherwise = GT

sortByLength :: [[a]] -> [[a]]
sortByLength xs = sortBy compareLength xs

intersperse' :: a -> [[a]] -> [a]
intersperse' s (x:xs)
             | null xs = x ++ []
             | otherwise = x ++ [s] ++ intersperse' s xs
intersperse' s [] = []

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving Show
              

testTreeRight = Node "root" (Node "l" Empty Empty) (Node "r" (Node "l" Empty (Node "r" Empty (Node "r" Empty Empty))) Empty)
testTreeLeft = Node "root" (Node "l" (Node "l" Empty Empty) Empty) (Node "r" (Node "l" Empty Empty) Empty)

heightOfTree :: Tree a -> Integer
heightOfTree Empty = 0
heightOfTree (Node _ Empty right) = 1 + heightOfTree right
heightOfTree (Node _ left Empty) =  1 + heightOfTree left
heightOfTree (Node _ left right)
             | heightOfTree left > heightOfTree right = 1 + heightOfTree left
             | otherwise = 1 + heightOfTree right

type Point = (Integer, Integer)

data Direction = DLeft | DRight
 deriving Show
 
slope :: Fractional a => Point -> Point -> a
slope (x0, y0) (x1, y1) = changeInY / changeInX
                        where changeInY = (fromInteger (y1 - y0))
                              changeInX = (fromInteger (x1 - x0))
                              
--simple method of finding direction of a next line:
findDirection :: Point -> Point -> Point -> Direction
findDirection p1 p2 p3  
              | mainSlope > destSlope = DRight
              | otherwise = DLeft
              where mainSlope = slope p1 p2
                    destSlope = slope p1 p3

findDirections :: [Point] -> [Direction]
findDirections xs
               | (length xs) == 2 = []
findDirections xxs@(p1:p2:p3:_) = findDirection p1 p2 p3 : findDirections (drop 1 xxs)

--that recursion will not throw stackoverflow
someFib n 
        | n <= 0 = 1
        | otherwise = n * someFib (n-1)
