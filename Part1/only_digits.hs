import Data.Char (isDigit)

onlyDigits [] = False
onlyDigits [x] = x>='0' && x<='9'
onlyDigits (x:xs)
   | x >= '0' && x <= '9' && length xs >= 1 = onlyDigits xs
   | length xs < 1 && x >= '0' && x <= '9' = True
   | otherwise = False

onlyDigits [] = False
onlyDigits (x:xs)
   | isDigit x && length xs >= 1 = onlyDigits xs
   | length xs < 1 && isDigit x = True
   | otherwise = False
