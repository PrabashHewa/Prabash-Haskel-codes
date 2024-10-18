module Phone_type2 (
    CountryCode(CountryCode),
    PhoneNo(PhoneNo),
    PhoneType(WorkLandline, PrivateMobile, WorkMobile, Other),
    Phone(Phone, phoneType, countryCode, phoneNo),
    toCountryCode,
    toPhoneNo
) where

data CountryCode = CountryCode Integer deriving (Eq, Show)
data PhoneNo = PhoneNo Integer deriving (Eq, Show)

-- | Convert an Integer to a CountryCode
toCountryCode :: Integer -> CountryCode
toCountryCode n
    | n < 0 = error "Negative country code"
    | otherwise = CountryCode n

-- | Convert an Integer to a PhoneNo
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
