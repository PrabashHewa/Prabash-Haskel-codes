calculate :: [String] -> [String]
calculate = map calculateLine

readMaybe :: Read a => String -> Maybe a
readMaybe str =
  case reads str of
    [(x,"")] -> Just x
    _        -> Nothing

calculateLine :: String -> String
calculateLine str =
  case words str of
    [x, "+", y] -> calculateOperation (+) x y
    [x, "-", y] -> calculateOperation (-) x y
    [x, "*", y] -> calculateOperation (*) x y
    _           -> "I cannot calculate that"

calculateOperation :: (Int -> Int -> Int) -> String -> String -> String
calculateOperation opStr x y =
  case (readMaybe x, readMaybe y) of
    (Just a, Just b) -> show (a `opStr` b)
    _                -> "I cannot calculate that"