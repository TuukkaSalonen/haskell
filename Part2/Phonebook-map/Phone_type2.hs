module Phone_type2
(Phone(..), PhoneType, CountryCode, PhoneNo, fromPhoneNo, toPhoneNo, readPhone, onlyDigits, removeExcess, checkPhoneType, toCountryCode, predefinedCountryCodes, phoneNro) where

data Phone = Phone {phoneType :: PhoneType, countryCode :: CountryCode, phoneNo :: PhoneNo} deriving (Eq, Ord)

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read, Ord)
data CountryCode = CountryCode Integer deriving (Eq, Ord)
data PhoneNo = PhoneNo Integer deriving (Eq, Ord, Read)

fromPhoneNo :: PhoneNo -> Integer
fromPhoneNo (PhoneNo x) = x

toPhoneNo :: Integer -> PhoneNo
toPhoneNo x
   | x < 0 = error "Negative phone number"
   | otherwise = PhoneNo x
   
readPhone :: String -> String -> String -> Phone
readPhone [] _ _ = error "Missing phone type"
readPhone _ [] _ = error "Empty country code"
readPhone _ _ [] = error "Empty phone number"
readPhone x y z
   | not (checkPhoneType x) = error "Incorrect phone type"
   | not ((read (removeExcess y)::Integer) `elem` predefinedCountryCodes) = error "Unknown country code"
   | not (onlyDigits (removeExcess y)) = error "Incorrect country code"
   | otherwise = Phone (read x :: PhoneType) (toCountryCode(read (removeExcess y) :: Integer)) (toPhoneNo(read z :: Integer))


onlyDigits [] = False
onlyDigits [x] = x>='0' && x<='9'
onlyDigits (x:xs)
   | x >= '0' && x <= '9' && length xs >= 1 = onlyDigits xs
   | length xs < 1 && x >= '0' && x <= '9' = True
   | otherwise = False
   
removeExcess (x:xs)
   | x == '+' = removeExcess (xs)
   | (x == '0' && head xs == '0') = removeExcess (tail xs)
   | otherwise = (x:xs)

checkPhoneType x
   | (x == show Other) || (x == show WorkLandline) || (x == show WorkMobile) || (x == show PrivateMobile) = True
   | otherwise = False

toCountryCode x
   | (fromInteger x) < 0 = error "Negative country code"
   | otherwise = CountryCode (fromInteger x)
   
predefinedCountryCodes = [44,358]

instance Show Phone where
   show (Phone x y z) = show y ++ " " ++ show z ++ " (" ++ show x ++ ")"
   
instance Show CountryCode where
   show (CountryCode x) = "+" ++ show x 
   
instance Show PhoneNo where
   show (PhoneNo x) = show x 
   
phoneNro x = phoneNo x
  