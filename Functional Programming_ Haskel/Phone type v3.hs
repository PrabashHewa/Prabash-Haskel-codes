-- Define newtype for CountryCode and PhoneNo
newtype CountryCode = CountryCode Integer deriving (Eq)
newtype PhoneNo = PhoneNo Integer deriving (Eq)

-- Define instances for Show for CountryCode and PhoneNo
instance Show CountryCode where
    show (CountryCode n) = '+' : show n

instance Show PhoneNo where
    show (PhoneNo n) = show n

-- Define conversion functions using newtype
toCountryCode :: Integer -> CountryCode
toCountryCode n
    | n < 0 = error "Negative country code"
    | otherwise = CountryCode n

toPhoneNo :: Integer -> PhoneNo
toPhoneNo n
    | n < 0 = error "Negative phone number"
    | otherwise = PhoneNo n

-- Define functions to convert from newtypes
fromPhoneNo :: PhoneNo -> Integer
fromPhoneNo (PhoneNo n) = n

-- Define Maybe for optional fields
data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other
                deriving (Show, Eq)

-- Use Maybe for optional fields in Phone
data Phone = Phone { phoneType :: Maybe PhoneType
                   , countryCode :: Maybe CountryCode
                   , phoneNo :: PhoneNo
                   } deriving (Eq)

-- Implement your own instance for Show for Phone
instance Show Phone where
    show (Phone mPhoneType mCountryCode phoneNo) =
        let countryCodeStr = case mCountryCode of
                                Just code -> show code ++ " "
                                Nothing -> ""
            phoneTypeStr = case mPhoneType of
                                Just pType -> " (" ++ show pType ++ ")"
                                Nothing -> ""
        in countryCodeStr ++ show phoneNo ++ phoneTypeStr

-- Implement the read functions
readPhoneType :: String -> Maybe PhoneType
readPhoneType "WorkLandline" = Just WorkLandline
readPhoneType "PrivateMobile" = Just PrivateMobile
readPhoneType "WorkMobile" = Just WorkMobile
readPhoneType "Other" = Just Other
readPhoneType _ = Nothing

readCountryCode :: String -> [Integer] -> Maybe CountryCode
readCountryCode "" _ = Nothing
readCountryCode str ccList
    | null str = Nothing
    | head str == '+' = Just (CountryCode (read (tail str)))
    | take 2 str == "00" = Just (CountryCode (read (drop 2 str)))
    | otherwise = let code = read str
                  in if code `elem` ccList
                        then Just (CountryCode code)
                        else Nothing

readPhoneNo :: String -> PhoneNo
readPhoneNo str
    | null str = error "Empty phone number"
    | not (all (`elem` "0123456789") str) = error "Incorrect phone number"
    | otherwise = toPhoneNo (read str)

readPhone :: String -> String -> String -> [Integer] -> Phone
readPhone phonetypestr countrycodestr phonenostr ccodelist =
    let phoneType' = if null phonetypestr then Nothing else readPhoneType phonetypestr
        countryCode' = readCountryCode countrycodestr ccodelist
        phoneNo' = readPhoneNo phonenostr
    in Phone phoneType' countryCode' phoneNo'
