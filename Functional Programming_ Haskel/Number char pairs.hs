charsDivisibleBy :: Int -> [Char]
charsDivisibleBy n = [toChar x | x <- [1..26], x `mod` n == 0]
  where
    toChar x = toEnum (x + 96) :: Char

charsProductOf :: [Int] -> [Char]
charsProductOf ns = [toChar x | x <- [1..26], any (\(a, b) -> a * b == x) pairs]
  where
    toChar x = toEnum (x + 96) :: Char
    pairs = [(a, b) | a <- ns, b <- ns, a /= b]