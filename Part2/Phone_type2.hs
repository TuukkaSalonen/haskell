data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read, Ord)

data CountryCode = CountryCode Int deriving (Eq, Ord)
data PhoneNo = PhoneNo Int deriving (Eq, Ord, Read)

data Phone = Phone {phoneType :: PhoneType, countryCode :: CountryCode, phoneNo :: PhoneNo} deriving (Eq, Ord)

instance Show Phone where
   show (Phone x y z) = show y ++ " " ++ show z ++ " (" ++ show x ++ ")"

makePhone x y z
   | y < 0 = error "Negative country code"
   | z < 0 = error "Negative phone number"
   | otherwise = Phone x y z
   
instance Show CountryCode where
   show (CountryCode x) = "+" ++ show x 

instance Num CountryCode where
   (+) (CountryCode x) (CountryCode y) = CountryCode (x+y)
   (-) (CountryCode x) (CountryCode y) = CountryCode (x-y)
   (*) (CountryCode x) (CountryCode y) = CountryCode (x*y)
   
instance Show PhoneNo where
   show (PhoneNo x) = show x 
   
instance Num PhoneNo where
   (+) (PhoneNo x) (PhoneNo y) = PhoneNo (x+y)
   (-) (PhoneNo x) (PhoneNo y) = PhoneNo (x-y)
   (*) (PhoneNo x) (PhoneNo y) = PhoneNo (x*y)

toCountryCode x
   | x < 0 = error "Negative country code"
   | otherwise = CountryCode (fromInteger x)
   
toPhoneNo x
   | x < 0 = error "Negative phone number"
   | otherwise = PhoneNo (fromInteger x)
   