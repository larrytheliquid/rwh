import Data.List
import Test.QuickCheck

lastButOne :: [a] -> a
lastButOne [] = error "wups"
lastButOne (x:_:[]) = x
lastButOne (_:xs) = lastButOne xs

data List a = Cons a (List a)
            | Nil
              deriving (Show)
                       
countWords :: [Char] -> Int                       
countWords w = length w'
  where w' = words w
                       
fromList (x:xs) = Cons x (fromList xs) 
fromList [] = Nil

toList (Cons x xs) = x : toList xs
toList Nil = []

prop_inverse_fromList :: [Int] -> Bool
prop_inverse_fromList xs = toList (fromList xs) == xs

tester x y z = x + y + z

-- Define a tree type that has only one constructor, like our Java example. Instead of the Empty 
-- constructor, use the Maybe type to refer to a node's children.
data MyTree a = MyNode (Maybe a) (Maybe (MyTree a)) (Maybe (MyTree a))
            deriving (Show)
                     
length' :: [a] -> Int
length' = foldr (\_ acc -> succ acc) 0

mean :: [Int] -> Double
mean xs = floatSum xs / floatLength xs
  where floatSum = fromIntegral . sum
        floatLength = fromIntegral . length
        
palindrome :: [a] -> [a]
palindrome xs = xs ++ reverse xs

sortByLength :: [[a]] -> [[a]]
sortByLength xs = sortBy comparedList xs
  where comparedList x y = compare (length x) (length y)
        
intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ (xs:[]) = xs
intersperse sep (xs:xss) = foldr appendSeparated xs xss
  where appendSeparated ys acc = acc ++ sep:ys
        
data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)
                     
height :: Tree a -> Int
height Empty = 0
height (Node _ l r)
  | leftHeight >= rightHeight = leftHeight
  | otherwise = rightHeight
    where leftHeight = succ (height l)
          rightHeight = succ (height r)

data Direction = DLeft | DRight | DStraight
data Point = Point { coordX :: Int, coordY :: Int }

turn :: Point -> Point -> Point -> Direction
turn a b c
  | crossProduct == 0 = DStraight
  | crossProduct > 0 = DLeft
  | crossProduct < 0 = DRight
  where leftCross = (coordX b - coordX a) * (coordY c - coordY a)
        rightCross = (coordY b - coordY a) * (coordX c - coordX a)
        crossProduct = leftCross - rightCross


main = quickCheck prop_inverse_fromList