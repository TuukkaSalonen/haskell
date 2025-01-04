charsDivisibleBy 0 = []
charsDivisibleBy x = [ y | xs <- [1..26], mod xs x == 0, let y = ['a'..'z'] !! (xs-1)]

charsProductOf [] = []
charsProductOf [x] = []
charsProductOf (x:xs) = [ e | ys <- xs, let y = x*ys, x>0, ys>0, y<27, let e = ['a'..'z'] !! (y-1)] ++ charsProductOf xs