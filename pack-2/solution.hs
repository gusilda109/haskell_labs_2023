sumOfDigits :: Int -> Int

sumOfDigits n
    | n < 10 = n
    | otherwise = n `mod` 10 + sumOfDigits(n `div` 10)

countDigits :: Int -> Int

countDigits n
    | n < 10 = 1
    | otherwise = 1 + countDigits(n `div` 10)

decToBin :: Int -> Int

decToBin n
    | n == 0 = 0
    | otherwise = n `mod` 2 + 10 * (decToBin(n `div` 2))

collatz :: Int -> Int

collatz n
    | n == 1 = 0
    | (n `mod` 2) == 0 = 1 + collatz(n `div` 2)  
    | (n `mod` 2) /= 0 = 1 + collatz((3*n) + 1)
