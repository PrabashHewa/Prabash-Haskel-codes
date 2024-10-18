-- Function to validate Finnish IBAN code
validate :: String -> Bool
validate iban =
  length iban == 18 &&     -- Check if IBAN length is correct
  take 2 iban == "FI" &&   -- Check if it begins with the country code FI
  all (\c -> c >= '0' && c <= '9') (drop 2 iban) &&  -- Check if all characters after country code are digits
  mod97 iban == 1          -- Check if the remainder of the IBAN integer division by 97 is 1

-- Function to calculate the remainder of IBAN integer division by 97
mod97 :: String -> Integer
mod97 iban = mod (read (rearrange iban) :: Integer) 97

-- Function to rearrange the IBAN string
rearrange :: String -> String
rearrange iban = drop 4 iban ++ take 4 iban ++ drop 2 iban

-- Main function to test the validate function
main :: IO ()
main = do
  putStrLn "Enter Finnish IBAN code:"
  iban <- getLine
  putStrLn $ "Is valid: " ++ show (validate iban)


