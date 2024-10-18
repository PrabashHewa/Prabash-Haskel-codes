validate :: String -> Bool
validate iban =
  length iban == 18 && 
  take 2 iban == "FI" &&
  isValidIBAN iban

isValidIBAN :: String -> Bool
isValidIBAN iban =
  let rearrangedIBAN = drop 4 iban ++ take 4 iban
      digitizedIBAN = map charToDigit rearrangedIBAN
      interpretedInteger = read digitizedIBAN :: Integer
  in interpretedInteger `mod` 97 == 1

charToDigit :: Char -> Char
charToDigit c
  | c `elem` ['0'..'9'] = c
  | otherwise = intToDigit (fromEnum c - fromEnum 'A' + 10)


