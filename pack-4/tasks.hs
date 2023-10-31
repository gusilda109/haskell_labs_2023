dotsInCircle :: (Double, Double) -> Double -> [(Double, Double)] -> [(Double, Double)]

dotsInCircle (x,y) r xs = [a | a <- xs, sqrt((fst a)^2 + (snd a)^2) <= r]

fizzbuzz :: [String]

fizzbuzz = [if (a `mod` 15 == 0) then "FizzBuzz" else if (a `mod` 5 == 0) then "Buzz" else if (a `mod` 3 == 0) then "Fizz" else show a| a <- [1..16]]

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

setAnd :: [Int] -> [Int] -> [Int]

setAnd xs ys = removeDuplicates [x | x <- xs, x `elem` xs && x `elem` ys]

sumDigits :: Int -> Int

sumDigits 0 = 0
sumDigits n = n `mod` 10 + sumDigits (n `div` 10)

countDigits :: Int -> Int

countDigits n
    | n < 10 = 1
    | otherwise = 1 + countDigits (n `div` 10)

isPow2 :: Int -> Int

isPow2 n
    | n == 1 = 1
    | n `mod` 2 == 0 = isPow2(n `div` 2)
    | otherwise = 0

sequenceByPred :: (a -> a) -> a -> [a]

sequenceByPred f x = x : sequenceByPred f (f x)

quicksort :: (Ord a) => [a] -> [a]    
quicksort [] = []    
quicksort (x:xs) =     
    let smallerSorted = quicksort (filter (<=x) xs)  
        biggerSorted = quicksort (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted

myLog :: Int -> Int

myLog 1 = 0
myLog n = head [k | k <- [0 .. ], 2^k > n]

sequenceByTwoPreds :: (a -> a -> a) -> a -> a -> [a]
sequenceByTwoPreds f x y = x : y : zipWith f (sequenceByTwoPreds f x y) (tail (sequenceByTwoPreds f x y))
