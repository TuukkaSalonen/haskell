import System.IO
import Data.Char
import Control.Monad
import Text.Read
import System.Directory  
import qualified Data.List as L

main = do
    putStrLn  "Welcome to phone book application"
    loop $ return []

loop :: IO PhoneBook -> IO ()
loop ioPhonebook =
 do
 input <- getLine
 if input == "quit"
   then putStrLn "bye"
   else doCommand input ioPhonebook

doCommand :: String -> IO PhoneBook -> IO ()
doCommand input ioPhonebook = do
    phonebook <- ioPhonebook
    let linewords = words input
    if ((length linewords < 2) || (length linewords > 5))
    then do
        putStrLn "Cannot do that"
        loop $ return phonebook 
    else do
        let command = (linewords !! 0)
            countofwords = length linewords
        if (command == "add") && (countofwords == 5)
        then do
            let uusi = addEntry (linewords !! 1) (linewords !! 2) (linewords !! 3) (linewords !! 4) phonebook
            putStrLn "Done"
            loop $ return uusi
        else if (command == "find") && (countofwords == 2)
        then do
            let foundentries = findEntries (linewords !! 1) phonebook
            if (length foundentries == 0)
            then do
                putStrLn "[]"
                loop $ return phonebook
            else do
                let itemstoprint = makeListOfPrints foundentries
                    commas = toCommaSeparatedString itemstoprint
                printElements commas
                loop $ return phonebook
        else do
            putStrLn "Cannot do that"
            loop $ return phonebook

toCommaSeparatedString :: [String] -> String
toCommaSeparatedString = L.intercalate ","

makeListOfPrints :: PhoneBook -> [String]
makeListOfPrints [] = []
makeListOfPrints (x:xs) = line : makeListOfPrints xs
                           where thephone = phone x
                                 line = show thephone

printElements :: String -> IO()
printElements items = putStrLn ("[" ++ items ++ "]")

--TUODUT FUNKTIOT:

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read, Ord)

data CountryCode = CountryCode Integer deriving (Eq, Ord)

data PhoneNo = PhoneNo Integer deriving (Eq, Ord)

data Phone = Phone { phoneType :: PhoneType,
                     countryCode :: CountryCode,
                     phoneNo :: PhoneNo
                     } deriving (Eq, Ord)


data PhoneBookEntry = PhoneBookEntry { name :: String , phone :: Phone } deriving(Eq, Ord, Show)

type PhoneBook = [PhoneBookEntry]

instance Show (CountryCode) where
         show (CountryCode x) =  "+" ++ show (fromInteger(x))
	 
instance Show (PhoneNo) where
         show (PhoneNo x) = show (fromInteger(x))
		 
instance Show (Phone) where
         show (Phone pt cc pn) = show(cc) ++ " " ++ show(pn) ++ " " ++ "(" ++ show(pt) ++ ")"
		 
instance Num CountryCode where
    (CountryCode x) + (CountryCode y) = CountryCode (x + y)
    (CountryCode x) - (CountryCode y) = CountryCode (x - y)
    (CountryCode x) * (CountryCode y) = CountryCode (x * y)
    fromInteger x = CountryCode x

instance Num PhoneNo where
    (PhoneNo x) + (PhoneNo y) = PhoneNo(x + y)
    (PhoneNo x) - (PhoneNo y) = PhoneNo(x - y)
    (PhoneNo x) * (PhoneNo y) = PhoneNo(x * y)
    fromInteger x = PhoneNo x
				 
toCountryCode :: Integer -> CountryCode
toCountryCode i
                | i < 0 = error "Negative country code"
                | otherwise = CountryCode i

toPhoneNo :: Integer -> PhoneNo
toPhoneNo pn
                | pn < 0 = error "Negative phone number"
                | otherwise = PhoneNo pn
					 

makePhone :: PhoneType -> CountryCode  -> PhoneNo -> Phone
makePhone pt cc pn
                  | cc < 0 = error "Negative country code"
                  | pn < 0 = error "Negative phone number"
				  | otherwise = Phone {phoneType = pt, countryCode = cc, phoneNo = pn}

readPhone :: String -> String -> String -> Phone
readPhone [] _ _ = error "Missing phone type"
readPhone _ [] _ = error "Empty country code"
readPhone _ _ [] = error "Empty phone number"
readPhone pt cc pn
                   | not (ptype `elem` [WorkLandline, PrivateMobile, WorkMobile, Other]) = error "Incorrect phone type"	
                   | not (onlyDigits codestring) = error "Incorrect country code"
                   | not (ccode `elem` pdcc) = error "Unknown country code"	
                   | not (onlyDigits pn) = error "Incorrect phone number"
                   | otherwise = makePhone ptype codereal pnumber
                    where ptype = read pt :: PhoneType
                          ccode = if cc !! 0 == '+' then read (tail cc) :: Integer
                                  else if cc !! 0 == '0' && cc !! 1 == '0' then read (drop 2 cc) :: Integer
                                  else read cc :: Integer
                          codestring = if cc !! 0 == '+' then tail cc
                                  else if cc !! 0 == '0' && cc !! 1 == '0' then drop 2 cc
                                  else cc
                          codereal = toCountryCode (ccode)
                          pnumber = toPhoneNo (read pn :: Integer)
                          pdcc = predefinedCountryCodes


findEntries :: String -> PhoneBook -> PhoneBook
findEntries [] [] = []
findEntries [] _ = []
findEntries _ [] = []
findEntries s pb = [ a | a <- pb, name a == s]

addEntry :: String -> String -> String -> String -> PhoneBook -> PhoneBook
addEntry [] _ _ _ pb = pb
addEntry _ [] _ _ pb = pb
addEntry _ _ [] _ pb = pb
addEntry _ _ _ [] pb = pb
addEntry n pt cc pn pb
                       | notfoundnumber == False = pb
                       | otherwise = PhoneBookEntry {name = n, phone = readPhone pt cc pn} : pb
                         where foundphones = findEntries n pb
                               notfoundnumber = findNumber thephonenum foundphones
                               thephonenum = toPhoneNo (read pn :: Integer)


findNumber :: PhoneNo -> PhoneBook -> Bool
findNumber _ [] = True
findNumber pn (x:xs) = if phoneNo (phone (x)) /= pn then findNumber pn xs else False

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

onlyDigits :: String -> Bool
onlyDigits [] = False
onlyDigits [s] = Main.isDigit s
onlyDigits (x:xs)
    | Main.isDigit x = onlyDigits xs
    | otherwise = False

predefinedCountryCodes = [44, 358]
