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
