-- Function to calculate the edit distance between two strings
editDistance :: String -> String -> Int
editDistance [] ys = length ys
editDistance xs [] = length xs
editDistance (x:xs) (y:ys)
  | x == y = editDistance xs ys
  | otherwise = 1 + minimum [editDistance xs (y:ys), editDistance (x:xs) ys, editDistance xs ys]

-- Function to calculate the Hamming distance between two strings
hammingDistance :: String -> String -> Int
hammingDistance [] [] = 0
hammingDistance (x:xs) (y:ys)
  | x == y = hammingDistance xs ys
  | otherwise = 1 + hammingDistance xs ys

-- Function to generate clusters of close strings
clusters :: (String -> String -> Float) -> Float -> [String] -> [[String]]
clusters f d ss = map (\s -> filter (\x -> f s x <= d) ss) ss

-- Example distance function 1
distance1 :: String -> String -> Float
distance1 s1 s2 = fromIntegral (editDistance s1 s2) / fromIntegral (max (length s1) (length s2))

-- Example distance function 2
distance2 :: String -> String -> Float
distance2 s1 s2 = fromIntegral (hammingDistance s1 s2) / fromIntegral (max (length s1) (length s2))

-- Examples
example1 = clusters distance1 0.3 ["aaabc", "aabdd", "a", "aa", "abdd", "bcbcb", "", "abcdefghij"]
example2 = clusters distance2 0.2 ["123a","456789b","45","abc", "ab1", "a12", "abcdefghij"]

main :: IO ()
main = do
  print example1
  print example2


{- Write a function clusters :: (String -> String -> Float) -> Float -> [String] -> [[String]] that is given:

f :: String -> String -> Float, a distance function like the ones in exercise Distance functions 1.

d :: Float, a distance limit

ss :: [String], a list of strings

For each string s in ss, the function clusters computes a “cluster”, which is a list of “close” strings in ss (strings that are at most distance d from the s). The list of strings close to s should also contain s (if the distance function allows).

The strings in clusters and the list of clusters may be in any order. The grader sorts them. -}
