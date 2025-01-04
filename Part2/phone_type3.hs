data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read, Ord)

newtype CountryCode = MakeCountryCode Integer deriving (Eq, Ord)
newtype PhoneNo = MakePhoneNo Integer deriving (Eq, Ord)

data Phone = Phone {phoneType :: Maybe PhoneType, countryCode :: Maybe CountryCode, phoneNo :: PhoneNo} deriving (Eq, Ord)

instance Show Phone where
   show (Phone Nothing Nothing z) = show z
   show (Phone Nothing (Just y) z) = show y ++ " " ++ show z
   show (Phone (Just x) Nothing z) = show z ++ " (" ++ show x ++ ")"
   show (Phone (Just x) (Just y) z) = show y ++ " " ++ show z ++ " (" ++ show x ++ ")"
   
instance Show CountryCode where
   show (MakeCountryCode x) = "+" ++ show (fromInteger(x)) 

instance Show PhoneNo where
   show (MakePhoneNo x) = show x 
   
instance Num CountryCode where
   fromInteger x = MakeCountryCode x
   (+) (MakeCountryCode x) (MakeCountryCode y) = MakeCountryCode (x+y)
   (-) (MakeCountryCode x) (MakeCountryCode y) = MakeCountryCode (x-y)
   (*) (MakeCountryCode x) (MakeCountryCode y) = MakeCountryCode (x*y)
   
   
instance Num PhoneNo where
   fromInteger x = MakePhoneNo x
   (+) (MakePhoneNo x) (MakePhoneNo y) = MakePhoneNo (x+y)
   (-) (MakePhoneNo x) (MakePhoneNo y) = MakePhoneNo (x-y)
   (*) (MakePhoneNo x) (MakePhoneNo y) = MakePhoneNo (x*y)

toCountryCode x
   | x < 0 = error "Negative country code"
   | otherwise = MakeCountryCode x
   
toPhoneNo x
   | x < 0 = error "Negative phone number"
   | otherwise = MakePhoneNo x

fromPhoneNo :: PhoneNo -> Integer
fromPhoneNo (MakePhoneNo x) = x

readPhoneType :: String -> Maybe PhoneType
readPhoneType [] = Nothing
readPhoneType x = Just (read x :: PhoneType)

readCountryCode :: String -> Maybe CountryCode
readCountryCode [] = Nothing
readCountryCode x = Just (toCountryCode (read x :: Integer))

readPhoneNo :: String -> PhoneNo
readPhoneNo x = toPhoneNo (read x :: Integer)

readPhone :: String -> String -> String -> Phone
readPhone [] [] z = Phone (Nothing) (Nothing) (readPhoneNo z)
readPhone [] y z = Phone (Nothing) (readCountryCode y) (readPhoneNo z)
readPhone x [] z = Phone (readPhoneType x) (Nothing) (readPhoneNo z)
readPhone x y z = Phone (readPhoneType x) (readCountryCode y) (readPhoneNo z)