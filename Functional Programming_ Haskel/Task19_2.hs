data CountryCode = CountryCode Integer deriving (Eq, Show)
data PhoneNo = PhoneNo Integer deriving (Eq, Show)

toCountryCode :: Integer -> CountryCode
toCountryCode n
    | n < 0 = error "Negative country code"
    | otherwise = CountryCode n

toPhoneNo :: Integer -> PhoneNo
toPhoneNo n
    | n < 0 = error "Negative phone number"
    | otherwise = PhoneNo n

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other
                deriving (Show, Eq)

data Phone = Phone { phoneType :: PhoneType
                   , countryCode :: CountryCode
                   , phoneNo :: PhoneNo
                   } deriving (Show, Eq)

readPhone :: String -> String -> String -> [Integer] -> Phone
readPhone phonetypestr countrycodestr phonenostr ccodelist =
    let phoneType' = readPhoneType phonetypestr
        countryCode' = readCountryCode countrycodestr ccodelist
        phoneNo' = readPhoneNo phonenostr
    in Phone phoneType' countryCode' phoneNo'

readPhoneType :: String -> PhoneType
readPhoneType phonetypestr
    | null phonetypestr = error "Missing phone type"
    | phonetypestr `notElem` ["WorkLandline", "PrivateMobile", "WorkMobile", "Other"] = error "Incorrect phone type"
    | otherwise = read phonetypestr

readCountryCode :: String -> [Integer] -> CountryCode
readCountryCode countrycodestr ccodelist
    | null countrycodestr = error "Empty country code"
    | not (all (`elem` "0123456789") countrycodestr) = error "Incorrect country code"
    | head countrycodestr == '+' = toCountryCode (read (tail countrycodestr))
    | take 2 countrycodestr == "00" = toCountryCode (read (drop 2 countrycodestr))
    | otherwise = let code = read countrycodestr
                  in if code `elem` ccodelist
                        then toCountryCode code
                        else error "Unknown country code"

readPhoneNo :: String -> PhoneNo
readPhoneNo phonenostr
    | null phonenostr = error "Empty phone number"
    | not (all (`elem` "0123456789") phonenostr) = error "Incorrect phone number"
    | otherwise = toPhoneNo (read phonenostr)

data PhoneBookEntry = PhoneBookEntry { name :: String , phone :: Phone } deriving (Eq, Show)
type PhoneBook = [PhoneBookEntry]

findEntries :: String -> PhoneBook -> PhoneBook
findEntries nameStr phonebook = filter (\entry -> nameStr == name (phone entry)) phonebook

addEntry :: String -> String -> String -> String -> [Integer] -> PhoneBook -> PhoneBook
addEntry name phonetype ccode phonenum ccodelist currentbook =
    let newPhone = readPhone phonetype ccode phonenum ccodelist
        existingEntry = findEntries name currentbook
    in if any (\entry -> phoneNo (phone entry) == phoneNo newPhone) existingEntry
           then currentbook
           else currentbook ++ [PhoneBookEntry name newPhone]

emptyBook :: PhoneBook
emptyBook = []



