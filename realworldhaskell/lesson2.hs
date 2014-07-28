data Thing = Shoe --type Thing with 5 data constructors
             | Ship
             | SealingWax
             | Cabbage
             | King
 deriving Show --deriving Show auto generates code for converting Thing to String

shoe :: Thing
shoe = Shoe

list'ofThing :: [Thing]
list'ofThing = [Shoe, Ship, SealingWax, Cabbage, King]

isSmall :: Thing -> Bool
isSmall Shoe = True
isSmall Ship = False
isSmall SealingWax = True
isSmall Cabbage = True
isSmall King = False

isSmall2 :: Thing -> Bool
isSmall2 Ship = False
isSmall2 King = False
isSmall2 _ = True

data FailableDouble = Failure
                     | OK Double
 deriving Show

ex01 = Failure
ex02 = OK 3.4

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d) = d

failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                     Failure -> 0
                     OK d -> d

--data constructors can have more than 1 arguments
data Person = Person String Int Thing
 deriving Show
 
brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a

showPersonWithName :: Person -> String
showPersonWithName p@(Person n _ _) = "Person " ++ show p ++ " with name:" ++ n

data Tree = Leaf Char
            | Node Tree Int Tree
            
tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))
