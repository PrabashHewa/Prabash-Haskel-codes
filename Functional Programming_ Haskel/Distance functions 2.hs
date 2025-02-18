distanceFilter :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
distanceFilter _ _ _ [] = []
distanceFilter distFunc d s ss = filter (\x -> distFunc s x <= d) ss

--Write a function distanceFilter :: (String -> String -> Float) -> Float -> String -> [String] -> [String] that, given a distance function f, a Float d, a String s and a list of Strings ss, returns all the strings in ss that are at most d distance away from s.