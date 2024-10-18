module Phone_book_tree (
    PhoneBook(Empty, Node),
    Name,
    addEntry,
    findEntries,
    emptyBook
) where

import Phone_type2

type Name = String

data PhoneBook = Empty | Node Name [Phone] PhoneBook PhoneBook deriving (Show, Eq)

-- | Add an entry to the phone book
addEntry :: Name -> String -> String -> String -> [Integer] -> PhoneBook -> PhoneBook
addEntry name phonetype countrycodestr phonenostr ccodelist Empty =
    Node name [Phone phoneType' countryCode' phoneNo'] Empty Empty
    where 
        phoneType' = readPhoneType phonetype
        countryCode' = readCountryCode countrycodestr ccodelist
        phoneNo' = readPhoneNo phonenostr
addEntry name phonetype countrycodestr phonenostr ccodelist (Node nodeName phones left right)
    | name == nodeName = Node nodeName (newPhone:phones) left right
    | name < nodeName = Node nodeName phones (addEntry name phonetype countrycodestr phonenostr ccodelist left) right
    | otherwise = Node nodeName phones left (addEntry name phonetype countrycodestr phonenostr ccodelist right)
    where newPhone = Phone phoneType' countryCode' phoneNo'
          phoneType' = readPhoneType phonetype
          countryCode' = readCountryCode countrycodestr ccodelist
          phoneNo' = readPhoneNo phonenostr

-- | Find all entries for a given name in the phone book
findEntries :: Name -> PhoneBook -> [Phone]
findEntries _ Empty = []
findEntries name (Node nodeName phones left right)
    | name == nodeName = phones
    | name < nodeName = findEntries name left
    | otherwise = findEntries name right

-- | Create an empty phone book
emptyBook :: PhoneBook
emptyBook = Empty
