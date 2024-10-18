countNotIn :: String -> String -> Int
countNotIn s1 s2 = length [c | c <- s1, c `notElem` s2]

distance1 :: String -> String -> Float
distance1 s1 s2
    | null s1 && null s2 = 0
    | otherwise = fromIntegral (countNotIn s1 s2 + countNotIn s2 s1) / fromIntegral (length s1 + length s2)

countNonDigits :: String -> Int
countNonDigits s = length [c | c <- s, not (c `elem` ['0'..'9'])]

distance2 :: String -> String -> Float
distance2 s1 s2
    | null s1 && null s2 = 0
    | otherwise = fromIntegral (countNonDigits s1 + countNonDigits s2) / fromIntegral (length s1 + length s2)


--given a pair (c1,c2), a gap g and a string s returns an Int telling how many times (c1,c2) appear in s with gap g.