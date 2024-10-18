gap :: (Char, Char) -> Int -> String -> Int
gap (c1, c2) g s = length [i | i <- [0..length s - 2 - g], s !! i == c1 && s !! (i + g + 1) == c2]

--Write a function gap :: (Char, Char) -> Int -> String -> Int that, given a pair (c1,c2), a gap g and a string s returns an Int telling how many times (c1,c2) appear in s with gap g.