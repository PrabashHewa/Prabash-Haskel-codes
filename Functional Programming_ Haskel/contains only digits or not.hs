onlyDigits :: String -> Bool
onlyDigits "" = False  
onlyDigits str = all (`elem` ['0'..'9']) str