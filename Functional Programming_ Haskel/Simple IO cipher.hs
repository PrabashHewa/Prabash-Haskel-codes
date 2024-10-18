import qualified Data.Map

encode :: Int -> String -> String
encode shift msg = map (charmap Data.Map.!) msg
  where charlist = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']
        listlength = length charlist
        shiftedlist = take listlength (drop (shift `mod` listlength) (cycle charlist))
        charmap = Data.Map.fromList $ zip charlist shiftedlist

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing

main :: IO ()
main = interactWithUser

interactWithUser :: IO ()
interactWithUser = do
    line <- getLine
    putStrLn ("> " ++ line)  -- Echo the input line here
    if line == "quit"
        then putStrLn "bye"
        else do
            let wordsList = words line
            case wordsList of
                ("encode" : shiftStr : rest) -> do
                    let maybeShift = readMaybe shiftStr :: Maybe Int
                    case maybeShift of
                        Just shift -> putStrLn $ unwords (map (encode shift) rest)
                        Nothing -> putStrLn "I cannot do that"
                ("decode" : shiftStr : rest) -> do
                    let maybeShift = readMaybe shiftStr :: Maybe Int
                    case maybeShift of
                        Just shift -> putStrLn $ unwords (map (decode shift) rest)
                        Nothing -> putStrLn "I cannot do that"
                _ -> putStrLn "I cannot do that"
            interactWithUser

