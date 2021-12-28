data Oblik = Krug Double | Pravougaonik Double Double deriving (Show, Eq)

povrsina :: Oblik -> Double
povrsina x = case x of
    (Krug a) -> a ^ 2 * 3.14
    (Pravougaonik a b) -> a * b

sumaPovrsina :: [Oblik] -> Double
sumaPovrsina x = case x of
    [] -> 0
    (x : xs) -> povrsina x + sumaPovrsina xs