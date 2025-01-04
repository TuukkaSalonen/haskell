data PhoneBookEntry = PhoneBookEntry { name :: String , phone :: Phone } deriving(Eq, Ord, Show)
type PhoneBook = [PhoneBookEntry]


findEntries :: String -> PhoneBook -> PhoneBook
findEntries n pBook = [xs | xs <- pBook, (name xs) == n]

addEntry :: String -> String -> String -> String -> PhoneBook -> PhoneBook
addEntry a b c d pBook
   | length (isEntry d (findEntries a pBook)) == 0 = PhoneBookEntry a (readPhone b c d) : pBook
   | otherwise = pBook

isEntry :: String -> PhoneBook -> PhoneBook
isEntry p pBook = [xs | xs <- pBook, phoneNo (phone xs) == toPhoneNo(read p :: Integer)]


data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read, Ord)

data CountryCode = CountryCode Int deriving (Eq, Ord)
data PhoneNo = PhoneNo Int deriving (Eq, Ord)

data Phone = Phone {phoneType :: PhoneType, countryCode :: CountryCode, phoneNo :: PhoneNo} deriving (Eq, Ord)

instance Show Phone where
   show (Phone x y z) = show y ++ " " ++ show z ++ " (" ++ show x ++ ")"

instance Show CountryCode where
   show (CountryCode x) = "+" ++ show x 

instance Num CountryCode where
   fromInteger x = fromInteger x
   (+) (CountryCode x) (CountryCode y) = CountryCode (x+y)
   (-) (CountryCode x) (CountryCode y) = CountryCode (x-y)
   (*) (CountryCode x) (CountryCode y) = CountryCode (x*y)
   
instance Show PhoneNo where
   show (PhoneNo x) = show x 
   
instance Num PhoneNo where
   fromInteger x = fromInteger x
   (+) (PhoneNo x) (PhoneNo y) = PhoneNo (x+y)
   (-) (PhoneNo x) (PhoneNo y) = PhoneNo (x-y)
   (*) (PhoneNo x) (PhoneNo y) = PhoneNo (x*y)

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
   | not ((read (removeExcess y)::Integer) `elem` [-1..10]) = error "Unknown country code"
   | not (onlyDigits (removeExcess y)) = error "Incorrect country code"
   | otherwise = Phone (read x :: PhoneType) (toCountryCode(read (removeExcess y) :: Integer)) (toPhoneNo(read z :: Integer))

removeExcess (x:xs)
   | x == '+' = removeExcess (xs)
   | (x == '0' && head xs == '0') = removeExcess (tail xs)
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
