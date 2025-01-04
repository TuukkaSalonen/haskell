data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read)

type CountryCode = Int
type PhoneNo = Int

data Phone = Phone {phoneType :: PhoneType, countryCode :: CountryCode, phoneNo :: PhoneNo} deriving (Show, Eq, Read)

makePhone x y z
   | y < 0 = error "Negative country code"
   | z < 0 = error "Negative phone number"
   | otherwise = Phone x y z