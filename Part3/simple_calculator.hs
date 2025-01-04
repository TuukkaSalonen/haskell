main = do
   line <- getLine
   let lasku = words line

   if (lasku !! 0) == "quit"
      then 
         putStrLn "bye"

   else if checkNum (lasku !! 0) == False || checkNum (lasku !! 2) == False || (length lasku) < 3
      then do
         putStrLn "I cannot calculate that"
         main
   
   else if lasku !! 1 == "+"
      then do 
         putStrLn (show ((read (lasku !! 0) :: Integer) + read (lasku !! 2) :: Integer))
         main
   
   else if lasku !! 1 == "-"
      then do 
         putStrLn (show ((read (lasku !! 0) :: Integer) - read (lasku !! 2) :: Integer))
         main
   
   else if lasku !! 1 == "*"
      then do 
         putStrLn (show ((read (lasku !! 0) :: Integer) * read (lasku !! 2) :: Integer))
         main
   else do
         putStrLn "I cannot calculate that"
         main

checkNum [] = False
checkNum [x] = x `elem` ['0'..'9'] || x == '-'
checkNum (x:xs)
   | (x `elem` ['0'..'9'] || x == '-' ) && length xs >= 1 = checkNum xs
   | length xs < 1 && (x `elem` ['0'..'9'] || x == '-') = True
   | otherwise = False
