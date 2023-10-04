

length' :: [Int] -> Int

length' [] = 0
length' [x] = 1
length' (x:xs) = 1 + length' xs

length'' :: [Int] -> Int

length'' xs = sum' [1 | _ <- xs] 

head' :: [Int] -> Int

head' (x:_) = x

tail' :: [Int] -> [Int]

tail' (_:x) = x

last' :: [Int] -> Int

last' [x] = x
last' (_:xs) = last' xs

init' :: [Int] -> [Int]

init' lst = [x | x <- lst, notElem x [last' lst]]

null' :: [Int] -> Bool

null' lst
    | lst == [] = True
    | otherwise = False

drop' :: Int -> [Int] -> [Int]

drop' _ [] = []
drop' 0 lst = lst
drop' n (_:xs) = drop' (n-1) xs


sum' :: [Int] -> Int

sum' [x] = x
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' :: [Int] -> Int

product' [] = 0
product' [x] = x
product' (x:xs) = x * product' xs

elem' :: Int -> [Int] -> Bool

elem' _ [] = False
elem' n (x:xs)            
    | n == x    = True
    | otherwise = elem' n xs