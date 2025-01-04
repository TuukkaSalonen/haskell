data Shape = Circle Float Float Float | Rectangle Float Float Float Float   

isNumber = (`elem` [1..9])

tuplaa x = x+x

tee2x f x = f (f x)

 
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 

surface (Circle _ _ r) = pi * r ^ 2  
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)  