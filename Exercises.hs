lastButOne :: [a] -> a
lastButOne (x:(_:[])) = x
lastButOne (x:xs) = lastButOne xs

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

tester x y z = x + y + z

-- Define a tree type that has only one constructor, like our Java example. Instead of the Empty 
-- constructor, use the Maybe type to refer to a node's children.
data Tree a = Node (Maybe a) (Maybe (Tree a)) (Maybe (Tree a))
            deriving (Show)