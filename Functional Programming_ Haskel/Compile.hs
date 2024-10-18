commonSubstring :: String -> String -> String
commonSubstring [] _ = []
commonSubstring _ [] = []
commonSubstring (x:xs) (y:ys)
    | x == y = x : commonSubstring xs ys
    | otherwise = dropWhile (/= x) ys

{- Write a function commonSubstring :: String -> String -> String that, given two strings s1 and s2, computes a common “substring” of s1 and s2 as follows. The function finds the earliest common character c (a character closest to the head of either s1 or s2, and appearing in both sequences). The function removes c and all the characters before it in both strings, puts c in the output string, and continues. -}

