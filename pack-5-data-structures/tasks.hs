quadraticSolver :: Double -> Double -> Double -> Maybe (Double, Double)
quadraticSolver a b c
  | discriminant < 0 = Nothing
  | otherwise = Just (root1, root2)
  where
    discriminant = b^2 - 4*a*c
    root1 = (-b + sqrt discriminant) / (2*a)
    root2 = (-b - sqrt discriminant) / (2*a)

maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

maybeTail :: [a] -> Maybe [a]
maybeTail []     = Nothing
maybeTail (_:xs) = Just xs

maybeInit :: [a] -> Maybe [a]
maybeInit []     = Nothing
maybeInit lst    = Just (init lst)

maybeFind :: (a -> Bool) -> [a] -> Maybe a
maybeFind _ [] = Nothing
maybeFind predicate (x:xs)
  | predicate x = Just x
  | otherwise   = maybeFind predicate xs
