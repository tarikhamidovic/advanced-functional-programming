{-# OPTIONS_GHC -Wall #-}

module MySet where

class ActsAsSet f where
  emp :: f a

  singleton :: a -> f a

  insert :: Ord a => a -> f a -> f a

  getElement :: Ord a => a -> f a -> Maybe a

  fold :: (a -> b -> b) -> b -> f a -> b

  delete :: Ord a => a -> f a -> f a
  delete x  = fold (\y s -> if x == y then s else insert y s) emp

  union :: Ord a => f a -> f a -> f a
  union = fold insert

  isEmpty :: f a -> Bool
  isEmpty x = size x == 0

  size :: f a -> Int
  size = fold (\_ y -> y + 1) 0

  isIn :: Ord a => a -> f a -> Bool
  isIn x = fold (\z s -> s || (z == x)) False

  toSortedList :: f a -> [a]
  toSortedList = fold (:) []


fromList :: (ActsAsSet f, Ord a) => [a] -> f a
fromList = fold insert emp

difference :: (ActsAsSet f, Ord a) => f a -> f a -> f a
difference = fold delete

subset :: (ActsAsSet f, Ord a) => f a -> f a -> Bool
subset xs ys = size (fold delete xs ys) == 0


instance ActsAsSet [] where
  emp = []
  singleton x = [x]
  getElement x y = if isIn x y then Just x else Nothing
  fold = foldr
  insert x y
    | y == [] = [x]
    | x < head y = [x] ++ y
    | x > last y = y ++ [x]
    | otherwise = fold (\z s -> if x > z && x < head s then [z] ++ [x] ++ s else [z] ++ s) [] y


data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show, Eq)

instance ActsAsSet Tree where
  emp = Empty
  singleton x = Node Empty x Empty
  getElement x y = if isIn x y then Just x else Nothing

  fold _ k Empty = k
  fold f k (Node left val right) = fold f (f val (fold f k right)) left

  insert x Empty = Node Empty x Empty
  insert x node@(Node left val right)
    | x < val = Node (insert x left) val right
    | x > val = Node left val (insert x right)
    | otherwise = node


newtype Pair k v = Pair { getTuple :: (k,v) }

instance Eq k => Eq (Pair k v) where
  (Pair kv1) == (Pair kv2) = fst kv1 == fst kv2

instance Ord k => Ord (Pair k v) where
  compare (Pair kv1) (Pair kv2) = compare (fst kv1) (fst kv2)

instance (Show k, Show v) => Show (Pair k v) where
  show (Pair (k,v)) = show k ++ " |-> " ++ show v

type Map f k v = f (Pair k v)
type ListMap k v = Map [] k v
type TreeMap k v = Map Tree k v

emptyMap :: ActsAsSet f => Map f k v
emptyMap = emp

extend :: (ActsAsSet f, Ord k) => k -> v -> Map f k v -> Map f k v
extend k v = insert (Pair (k, v))

toSList :: ActsAsSet f => Map f k v -> [(k,v)]
toSList = fold (\x y -> getTuple x : y) []

lookup :: (ActsAsSet f, Ord k) => k -> Map f k v -> Maybe v
lookup k = fold (\y s -> if fst (getTuple y) == k then Just (snd (getTuple y)) else s) Nothing

remove :: (ActsAsSet f, Ord k) => k -> Map f k v -> Map f k v
remove k = fold (\y s -> if fst (getTuple y) == k then s else insert y s) emp