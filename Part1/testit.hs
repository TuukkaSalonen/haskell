tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y 

length' [] = 0  
length' (_:xs) = 1 + length' xs   

capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital (x:xs) = "The first letter of " ++ (x:xs) ++ " is " ++ show x

max' a b   
    | a > b     = a  
    | otherwise = b  

bmi paino pituus
  | bmi <= skinny = "Laiha"
  | bmi <= normal = "Normaali"
  | bmi <= fat = "Lihava"
  | otherwise = "Lehmä"
  where bmi = paino / pituus ^2
	skinny = 18.5
	normal = 25.0
	fat = 30.0

calcBmis xs = [bmi w h | (w, h) <- xs]  
  where bmi weight height = weight / height ^ 2 


describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list"