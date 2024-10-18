module Phone_book_map
  ( PhoneBook,
    Name,
    findEntries,
    addEntry,
    emptyBook
  ) where

import qualified Data.Map as Map
import Phone_type2

type Name = String
type PhoneBook = Map.Map Name [Phone]

findEntries :: Name -> PhoneBook -> [Phone]
findEntries name book = Map.findWithDefault [] name book

addEntry :: Name -> String -> String -> String -> [Integer] -> PhoneBook -> PhoneBook
addEntry name phonetype countrycode phoneno ccodelist book =
  let phone = readPhone phonetype countrycode phoneno ccodelist
   in Map.insertWith (++) name [phone] book

emptyBook :: PhoneBook
emptyBook = Map.empty
