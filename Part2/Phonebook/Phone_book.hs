module Phone_book (addEntry, findEntries) where

import Phone_type2

data PhoneBookEntry = PhoneBookEntry { name :: String , phone :: Phone } deriving(Eq, Ord, Show)
type PhoneBook = [PhoneBookEntry]

addEntry :: String -> String -> String -> String -> PhoneBook -> PhoneBook
addEntry a b c d pBook
   | length (isEntry d (findEntries a pBook)) == 0 = PhoneBookEntry a (readPhone b c d) : pBook
   | otherwise = pBook

findEntries :: String -> PhoneBook -> PhoneBook
findEntries n pBook = [xs | xs <- pBook, (name xs) == n]

isEntry :: String -> PhoneBook -> PhoneBook
isEntry p pBook = [xs | xs <- pBook, phoneNo (phone xs) == toPhoneNo(read p :: Integer)]
