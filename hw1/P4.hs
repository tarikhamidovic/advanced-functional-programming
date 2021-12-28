integerUlistuCifri :: Integer -> [Integer]
integerUlistuCifri a = map (\x -> read [x]) (show a)

-- zadnjaCifra vraća zadnju cifru u broju
zadnjaCifra :: Integer -> Integer
zadnjaCifra a = last (integerUlistuCifri a)

-- izbaciZadnjuCifru eliminira zadnju cifru iz broja
izbaciZadnjuCifru :: Integer -> Integer
izbaciZadnjuCifru a = div a 10

-- uCifre konvertuje broj u listu njegovih cifri 
-- na način da se na početku liste nalazi zadnja cifra,
-- nakon nje predzadnja, itd ...
uCifre :: Integer -> [Integer]
uCifre a = reverse (integerUlistuCifri a)

-- duplirajSvakiDrugi duplira svaki drugi element liste
duplirajSvakiDrugi :: [Integer] -> [Integer]
duplirajSvakiDrugi a = case a of
    [] -> []
    [x] -> [x]
    (x:y:xs) -> x : y * 2 : duplirajSvakiDrugi xs

-- sumirajSveCifre implementira treći korak u algoritmu
sumirajSveCifre :: [Integer] -> Integer
sumirajSveCifre a = case a of
    [] -> 0
    (x : xs) -> sum (integerUlistuCifri x) + sumirajSveCifre xs

verificiraj :: Integer -> Bool 
verificiraj a
    | izraz `mod` 10 == 0 = True 
    | otherwise = False
    where 
        izraz = sumirajSveCifre (duplirajSvakiDrugi (uCifre a))