validate [] = False
validate xs
   | x <- xs, length x == 18, (x!!0 == 'F' && x!!1 == 'I'), all isDigit(drop 2 x), ((read(loppuun(korvaa x)) :: Integer) `mod` 97 == 1) = True
   | otherwise = False

loppuun xs = drop 6 xs ++ take 6 xs

korvaa xs = '1':'5':'1':'8': drop 2 xs

isDigit x = x>='0' && x<='9'
 