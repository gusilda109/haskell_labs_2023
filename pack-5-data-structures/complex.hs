module ComplexNumbers where

data Number = Number { realPart :: Double, imagPart :: Double }
    deriving (Show)

add :: Number -> Number -> Number
add (Number a1 b1) (Number a2 b2) = Number (a1 + a2) (b1 + b2)

subtract :: Number -> Number -> Number
subtract (Number a1 b1) (Number a2 b2) = Number (a1 - a2) (b1 - b2)

multiply :: Number -> Number -> Number
multiply (Number a1 b1) (Number a2 b2) =
    Number (a1 * a2 - b1 * b2) (a1 * b2 + b1 * a2)

divide :: Number -> Number -> Number
divide (Number r1 i1) (Number r2 i2) =
    Number ((r1 * r2 + i1 * i2) / denominator) ((i1 * r2 - r1 * i2) / denominator)
    where denominator = r2^2 + i2^2

conjugate :: Number -> Number
conjugate (Number a b) = Number a (-b)

absComplex :: Number -> Double
absComplex (Number a b) = sqrt (a^2 + b^2)