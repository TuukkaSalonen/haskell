commonSubstring [] _ = []
commonSubstring _ [] = []
commonSubstring (x:xs) ys
   | x `elem` ys = [x] ++ commonSubstring xs (tail (fString ys x))
   | otherwise = commonSubstring xs ys

fString [] _ = []
fString [x] c = [x]
fString (x:xs) c
   | x == c = x:xs
   | otherwise = fString xs c