import Data.Char

main = do

   line <- getLine
   let rivi = words line

   if ((length rivi > 0) && rivi !! 0 == "quit")
      then 
        putStrLn "bye"
   else if ((length rivi > 2) && (rivi !! 0 == "encode") && checkNum(rivi !! 1))
      then do
         let luku = read (rivi !! 1) :: Int
         let mjt = unwords (mjEncode (drop 2 rivi) luku)
         putStrLn mjt
         main
   else if ((length rivi > 2) && (rivi !! 0 == "decode") && checkNum(rivi !! 1))
      then do
         let luku = read (rivi !! 1) :: Int
         let mjt = unwords (mjDecode (drop 2 rivi) luku)
         putStrLn mjt
         main
   else do
      putStrLn "I cannot do that"
      main

encode :: Int -> String -> String 
encode shift msg = 
 let ords = map ord msg 
     shifted = map (+ shift) ords 
 in map chr shifted

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

checkNum [] = False
checkNum [x] = x `elem` ['0'..'9'] || x == '-'
checkNum (x:xs)
   | (x `elem` ['0'..'9'] || x == '-' ) && length xs >= 1 = checkNum xs
   | length xs < 1 && (x `elem` ['0'..'9'] || x == '-') = True
   | otherwise = False

mjEncode xs z = map (\x -> encode z x) xs
mjDecode xs z = map (\x -> decode z x) xs