credits :: (Char, Int) -> (Char, Int) -> Int 
credits (a,b) (c,d)
  | ((a == 's' && b == 14) || (c == 's' && d == 14)) = 14
  | (a == c && abs(b-d)==1) = 8
  | (b == d) = 6
  | (abs(b-d)==1) = 4
  | (a == c) = 2
  | otherwise = 0