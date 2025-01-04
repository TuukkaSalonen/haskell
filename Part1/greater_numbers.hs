
nextIsGreater :: [Int] -> [Int]
nextIsGreater xs = [x | (x,y) <- zip (init xs) (tail xs), x<y]



