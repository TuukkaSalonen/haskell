distance1 [] [] = 0
distance1 xs ys = ((notIn xs ys) + (notIn ys xs)) / ((fromIntegral (length xs) :: Float) + (fromIntegral (length ys) :: Float))

notIn [] _ = 0
notIn (x:xs) ys
   | elem x ys = notIn xs ys
   | otherwise = 1 + notIn xs ys
   
notNums [] = 0
notNums [x]
   | elem x ['0'..'9'] = 0
   | otherwise = 1
notNums (x:xs)
   | elem x ['0'..'9'] = notNums xs
   | otherwise = 1 + notNums xs
   
distance2 [] [] = 0
distance2 xs ys = ((notNums xs) + (notNums ys)) / ((fromIntegral (length xs) :: Float) + (fromIntegral (length ys) :: Float))