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
readPhone phonetypestr countrycodestr phonenostr ccodelist
    | null phonetypestr = error "Missing phone type"
    | not (isValidPhoneType phonetypestr) = error "Incorrect phone type"
    | null countrycodestr = error "Empty country code"
    | not (isValidCountryCode countrycodestr ccodelist) = error "Unknown country code"
    | null phonenostr = error "Empty phone number"
    | not (isValidPhoneNumber phonenostr) = error "Incorrect phone number"
    | otherwise = Phone (readPhoneType phonetypestr) (toCountryCode $ readCleanCountryCode countrycodestr) (toPhoneNo $ read phonenostr)

isValidPhoneType :: String -> Bool
isValidPhoneType phonetypestr = phonetypestr `elem` ["WorkLandline", "PrivateMobile", "WorkMobile", "Other"]

isValidCountryCode :: String -> [Integer] -> Bool
isValidCountryCode countrycodestr ccodelist =
    case cleanCountryCode countrycodestr of
        Just code -> code `elem` ccodelist
        Nothing -> False

cleanCountryCode :: String -> Maybe Integer
cleanCountryCode str
    | '+' `elem` str = readMaybe (tail str)
    | "00" `isPrefixOf` str = readMaybe (drop 2 str)
    | otherwise = readMaybe str

readCleanCountryCode :: String -> Integer
readCleanCountryCode str =
    case cleanCountryCode str of
        Just code -> code
        Nothing -> error "Incorrect country code"

readPhoneType :: String -> PhoneType
readPhoneType "WorkLandline" = WorkLandline
readPhoneType "PrivateMobile" = PrivateMobile
readPhoneType "WorkMobile" = WorkMobile
readPhoneType "Other" = Other
readPhoneType _ = error "Incorrect phone type"

isValidPhoneNumber :: String -> Bool
isValidPhoneNumber = all isDigit

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(val, "")] -> Just val
    _           -> Nothing

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'


