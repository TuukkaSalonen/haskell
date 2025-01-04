
data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read, Ord)

data CountryCode = CountryCode Int deriving (Eq, Ord)
data PhoneNo = PhoneNo Int deriving (Eq, Ord)

data Phone = Phone {phoneType :: PhoneType, countryCode :: CountryCode, phoneNo :: PhoneNo} deriving (Eq, Ord)

instance Show Phone where
   show (Phone x y z) = show y ++ " " ++ show z ++ " (" ++ show x ++ ")"

instance Show CountryCode where
   show (CountryCode x) = "+" ++ show x 

instance Num CountryCode where
   (CountryCode x) + (CountryCode y) = CountryCode (x+y)
   (CountryCode x) - (CountryCode y) = CountryCode (x-y)
   (CountryCode x) * (CountryCode y) = CountryCode (x*y)
   
instance Show PhoneNo where
   show (PhoneNo x) = show x 
   
instance Num PhoneNo where
   (PhoneNo x) + (PhoneNo y) = PhoneNo (x+y)
   (PhoneNo x) - (PhoneNo y) = PhoneNo (x-y)
   (PhoneNo x) * (PhoneNo y) = PhoneNo (x*y)

toCountryCode x
   | (fromInteger x) < 0 = error "Negative country code"
   | otherwise = CountryCode (fromInteger x)
   
toPhoneNo x
   | (fromInteger x) < 0 = error "Negative phone number"
   | otherwise = PhoneNo (fromInteger x)

readPhone [] _ _ = error "Missing phone type"
readPhone _ [] _ = error "Empty country code"
readPhone _ _ [] = error "Empty phone number"
readPhone x y z
   | not (checkPhoneType x) = error "Incorrect phone type"
   | not ((read (removeExcess y) :: Integer) `elem` [-1..10]) = error "Unknown country code"
   | not (onlyDigits (removeExcess y)) = error "Incorrect country code"
   | not (onlyDigits z) = error "Incorrect phone number"
   | otherwise = Phone (read x :: PhoneType) (toCountryCode(read (removeExcess y) :: Integer)) (toPhoneNo(read z :: Integer))

removeExcess (x:xs)
   | x == '+' = removeExcess (xs)
   | (x == '0' && head xs == '0') = tail xs
   | otherwise = (x:xs) 
   
checkPhoneType x
   | (x == show Other) || (x == show WorkLandline) || (x == show WorkMobile) || (x == show PrivateMobile) = True
   | otherwise = False

onlyDigits [] = False
onlyDigits [x] = x>='0' && x<='9'
onlyDigits (x:xs)
   | x >= '0' && x <= '9' && length xs >= 1 = onlyDigits xs
   | length xs < 1 && x >= '0' && x <= '9' = True
   | otherwise = False