{-# LANGUAGE DeriveFoldable #-}

import Data.Maybe
import Data.Char
import qualified Data.Text as T
import Data.List (intercalate, nub)
import Data.Char (isDigit)


-- Trees
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving Foldable

data Cat = Cat

tree1 :: Tree (Maybe Cat)
tree1 =
  Node
    Empty
    Nothing
    (Node
       (Node (Leaf Nothing) Nothing (Node Empty Nothing (Leaf (Just Cat))))
       Nothing
       (Node (Leaf Nothing) Nothing Empty))

-- Find Cat
findCat :: Tree (Maybe Cat) -> Int
findCat tree = length (fromJust (wayToCat tree)) + 1

-- Way to Cat
wayToCat :: Tree (Maybe Cat) -> Maybe [String]
wayToCat Empty = Nothing
wayToCat (Leaf (Just Cat)) = Just []
wayToCat (Node left _ right) =
  case (wayToCat left, wayToCat right) of
    (Just path, _) -> Just ("l" : path)
    (_, Just path) -> Just ("r" : path)
    _ -> Nothing

-- Casino
data Suit = Hearts | Tiles | Clovers | Pikes deriving (Show, Eq, Ord)

type Value = Int

data Card = JokerA | JokerB | Card Suit Value deriving (Show, Eq, Ord)

-- Card Pikes (-1)
deck1 :: [Card]
deck1 = [JokerA, JokerB] ++ [Card Hearts x | x <- [1 .. 13]]

fullDeck :: [Card]
fullDeck =
  [JokerA, JokerB]
    ++ [Card Hearts x | x <- [1 .. 13]]
    ++ [Card Tiles x | x <- [1 .. 13]]
    ++ [Card Clovers x | x <- [1 .. 13]]
    ++ [Card Pikes x | x <- [1 .. 13]]

-- Check if Deck is Full
isDeckFull :: [Card] -> Bool
isDeckFull deck
  | length deck /= 54 = False
  | otherwise = all (`elem` deck) fullDeck

-- Power Set
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = map (x :) (powerset xs) ++ powerset xs

-- Split String
splitString :: String -> [T.Text]
splitString str = T.splitOn (T.pack ".") (T.pack str)

-- Symbol to Integer
symbolToInteger :: Char -> Int
symbolToInteger sym
  | ord sym > 64 && ord sym < 91 = ord sym - 55
  | ord sym > 96 && ord sym < 123 = ord sym - 87
  | ord sym > 47 && ord sym < 58 = ord sym - 48
  | otherwise = error "патамушта понабирают всяких"

-- String to Int
stringToInt :: String -> Int
stringToInt str =
  foldl
    (\acc c -> (acc + symbolToInteger c) * 10)
    0
    (init str)
    + symbolToInteger (last str)

-- Check if IP Segment is Valid
isSegmentValid :: T.Text -> Bool
isSegmentValid segment
  | length (T.unpack segment) <= 3
      && all isDigit (T.unpack segment)
      && stringToInt (T.unpack segment) <= 255
      && stringToInt (T.unpack segment) >= 0 =
    True
  | otherwise = False

-- Validate IPV4
validIPV4 :: String -> Bool
validIPV4 str
  | all isSegmentValid (splitString str)
      && length (splitString str) == 4 =
    True
  | otherwise = False

-- Generate IPV4
generateIPV4 :: String -> Int -> [String]
generateIPV4 _ 0 = []
generateIPV4 [] _ = []
generateIPV4 [x] _ = [[x]]
generateIPV4 (x:xs) n =
    map (++ ".") (generateIPV4 xs (n - 1)) ++ map (x :) (generateIPV4 xs n)

-- Generate Valid IPV4
generateValidIPV4 :: String -> [String]
generateValidIPV4 str = filter validIPV4 $ generateIPV4 str 4

-- Count Occurrences of an Element in a List
aCount :: (Eq a) => [a] -> a -> Int
aCount list el = length (filter (== el) list)

-- Generate String of "|"
generateString :: Int -> String
generateString 0 = []
generateString num = generateString (num - 1) ++ "|"

-- Make Histogram
makeHistogram :: (Eq a, Show a) => [a] -> String
makeHistogram list =
  "Total: " ++ show (length list) ++ " elem" ++ "\n" ++
  intercalate "\n" (map (\el -> show el ++ ": " ++ generateString (aCount list el)) (nub list))

-- My Matrix
myMatrix :: [[Int]] -> [[Int]]
myMatrix ([]:_) = []
myMatrix mtrx = map head mtrx : myMatrix (map tail mtrx)

-- Sum of Squares of Digits
sumQuadr :: Int -> Int
sumQuadr num
  | num < 1 = num
  | otherwise = (num `mod` 10) ^ 2 + sumQuadr (num `div` 10)

-- Check if a Number is Nice
niceNumber :: Int -> Bool
niceNumber num
  | num == 1 = True
  | niceNumber (sumQuadr num) = True
  | otherwise = False
