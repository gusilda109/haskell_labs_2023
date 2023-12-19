--уравнение
quadraticSolver :: Double -> Double -> Double -> Maybe (Double, Double)
quadraticSolver a b c
  | discriminant < 0 = Nothing
  | otherwise = Just ((-b + sqrt discriminant) / (2 * a), (-b - sqrt discriminant) / (2 * a))
  where
    discriminant = b^2 - 4 * a * c


--функции
maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

maybeTail :: [a] -> Maybe [a]
maybeTail []     = Nothing
maybeTail (_:xs) = Just xs

maybeInit :: [a] -> Maybe [a]
maybeInit []     = Nothing
maybeInit [x]    = Just []
maybeInit (x:xs) = fmap (x :) (maybeInit xs)

maybeFind :: (a -> Bool) -> [a] -> Maybe a
maybeFind _ []                 = Nothing
maybeFind predicate (x:xs)
  | predicate x  = Just x
  | otherwise    = maybeFind predicate xs


--собаки
-- Определение типа данных DogBreed
data DogBreed
  = GoldenRetrievers
  | BostonTerriers
  | LabradorRetrievers
  | Poodles
  | BorderCollie
  | Beagle
  | IrishSetter
  | Staffordshire
  | Bull
  | Terrier
  deriving (Show, Eq)

-- Определение типа данных Gender
data Gender = Male | Female
  deriving (Show, Eq)

-- Определение типа данных Dog
data Dog = Dog
  { name :: String
  , age :: Int
  , gender :: Gender
  , breed :: DogBreed
  , isGoodBoy :: Bool
  }
  deriving (Show, Eq)

-- Список собак
dogs :: [Dog]
dogs =
  [ Dog "Leander" 12 Male Beagle False
  , Dog "Ouranos" 1 Male Poodles True
  , Dog "Pegasus" 2 Female Beagle False
  , Dog "Atlas" 8 Female GoldenRetrievers True
  , Dog "Castor" 6 Male LabradorRetrievers True
  , Dog "Apollo" 3 Female Beagle False
  , Dog "Narkissos" 15 Male Beagle True
  , Dog "Dardanos" 7 Female Terrier True
  , Dog "Ajax" 4 Male IrishSetter False
  , Dog "Pyrrhos" 2 Female BorderCollie False
  , Dog "Patroclus" 6 Male Bull True
  , Dog "Iacchus" 4 Female Beagle True
  ]

-- Определение функций, использующих собак
goodBoys :: [Dog]
goodBoys = filter isGoodBoy dogs

longNamedDogs :: [Dog]
longNamedDogs = filter (\dog -> length (name dog) > 7) dogs

mostPopularDogGender :: Gender
mostPopularDogGender =
  let maleCount = length $ filter (\dog -> gender dog == Male) dogs
      femaleCount = length $ filter (\dog -> gender dog == Female) dogs
  in if maleCount >= femaleCount then Male else Female

oldestDog :: Dog
oldestDog = foldl1 (\acc dog -> if age dog > age acc then dog else acc) dogs



--mylist
data MyList a = EmptyList | ListNode a (MyList a) deriving Show

fromList :: [a] -> MyList a
fromList []     = EmptyList
fromList (x:xs) = ListNode x (fromList xs)

toList :: MyList a -> [a]
toList EmptyList        = []
toList (ListNode x xs) = x : toList xs

reverseMyList :: MyList a -> MyList a
reverseMyList list = reverseMyList' list EmptyList
  where
    reverseMyList' EmptyList acc        = acc
    reverseMyList' (ListNode x xs) acc = reverseMyList' xs (ListNode x acc)

mapMyList :: (a -> b) -> MyList a -> MyList b
mapMyList _ EmptyList        = EmptyList
mapMyList f (ListNode x xs) = ListNode (f x) (mapMyList f xs)
