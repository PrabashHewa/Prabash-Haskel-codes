headOrLast :: [String] -> Char -> [String]
headOrLast strings ch = filter (\s -> head s == ch || last s == ch) strings