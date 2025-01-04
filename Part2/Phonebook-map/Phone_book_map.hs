module Phone_book_map (Name, Phonebook, addEntry, findEntries, isEntry, getPhones) where

import Phone_type2
import qualified Data.Map as Map

type Name = String
type Phonebook = Map.Map Name [Phone]

addEntry :: Name -> String -> String -> String -> Phonebook -> Phonebook
addEntry a b c d pBook
   | not (isEntry a pBook) = Map.insert a [readPhone b c d] pBook
   | not ((toPhoneNo (read d :: Integer)) `elem` (getPhones (findEntries a pBook))) = Map.adjust ((readPhone b c d):) a pBook
   | otherwise = pBook

findEntries :: Name -> Phonebook -> [Phone]
findEntries n pBook = Map.findWithDefault [] n pBook

getPhones xs = [x | y <- xs, let x = phoneNro y]

isEntry :: Name-> Phonebook -> Bool
isEntry p pBook = Map.member p pBook