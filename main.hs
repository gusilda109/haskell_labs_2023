-- Write a function that accepts three arguments (a,b,c) and returns a solution to the quadratic equasion ax^2 + bx + c = 0
-- The return type should be a pair of floating-point numbers. In case there is no solution, function should throw error (see example below) 
-- Also, write the type of solveQuadratic function
-- solveQuadratic ::

solveQuadratic :: Float -> Float -> Float -> (Float, Float)

solveQuadratic a b c = if (b ^ 2 - 4 * a * c < 0) then error "ERROR!!!!!!ASHIBKA" 
else ((-(b) - sqrt(b ^ 2 - 4 * a * c))/ (2 * a), (-(b) + sqrt(b ^ 2 - 4 * a * c))/(2 * a))

main :: IO()
main = do
    let result = solveQuadratic (-1) 1 (-8)
    print result