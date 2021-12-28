fr :: (a -> b -> b) -> b -> [a] -> b
fr _ k [] = k
fr f k (x : xs) = f x (fr f k xs)

fl :: (b -> a -> b) -> b -> [a] -> b
fl _ k [] = k
fl f k (x : xs) = fl f (f k x) xs

myReverse :: [a] -> [a]
myReverse a = fl (\k x -> x : k) [] a