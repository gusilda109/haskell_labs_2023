
module MyPrintf where

import Data.Char(digitToInt)


myPrintf :: (Show a) => String -> [a] -> String
myPrintf "" _ = ""
myPrintf ('%':'s':rest) (arg:args) = show arg ++ myPrintf rest args
myPrintf (c:rest) args = c : myPrintf rest args

main :: IO ()
main = do
  let a = myPrintf "Hello, world!" ["World"]
  putStrLn a


decToBin' :: Int -> [Int]

decToBin' n
    | n == 1 = [1]
    | n == 0 = [0]
    | otherwise = decToBin'(n `div` 2)  ++ [n `mod` 2]

baseNToDecimal :: Integer -> [Integer] -> Integer
baseNToDecimal base digits = foldl (\acc x -> acc * base + x) 0 digits

strToInt :: String -> Int
strToInt str = foldl (\acc x -> acc * 10 + digitToInt x) 0 str

findMissing :: [Int] -> Int
findMissing xs = findMissing' 1 xs
  where
    findMissing' n [] = n
    findMissing' n (x:xs)
      | n == x = findMissing' (n + 1) xs
      | otherwise = n

parenthesesChecker :: String -> Bool
parenthesesChecker s = checker s 0

checker :: String -> Int -> Bool
checker [] 0 = True
checker [] _ = False
checker ('(':xs) count = checker xs (count + 1)
checker (')':xs) count
  | count > 0  = checker xs (count - 1)
  | otherwise = False
checker (_:xs) count = checker xs count


