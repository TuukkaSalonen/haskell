gap _ _ [] = 0
gap (a,b) vali (x:xs)
   | (length xs > 0 && a == x && head(drop vali xs) == b) = 1 + gap (a,b) vali (xs)
   | length xs>1 = gap (a,b) vali (xs)
   | otherwise = 0

jonkun takia vaati gap _ _ [] = 0 lisäks length xs > 0? et meni kaikista läpi
