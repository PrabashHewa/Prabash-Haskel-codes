commonSubstring :: String -> String -> String
commonSubstring [] _ = []
commonSubstring _ [] = []
commonSubstring (x:xs) (y:ys)
    | x == y = x : commonSubstring xs ys
    | otherwise = []