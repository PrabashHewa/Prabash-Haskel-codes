module Phone_book
  ( PhoneBook,
    findEntries,
    addEntry,
    emptyBook
  ) where

import Phone_type2

type PhoneBook = [(String, Phone)]

findEntries :: String -> PhoneBook -> PhoneBook
findEntries name book = filter (\(n, _) -> n == name) book

addEntry :: String -> String -> String -> String -> [Integer] -> PhoneBook -> PhoneBook
addEntry name phonetype countrycode phoneno ccodelist book =
  let phone = readPhone phonetype countrycode phoneno ccodelist
   in (name, phone) : book

emptyBook :: PhoneBook
emptyBook = []
