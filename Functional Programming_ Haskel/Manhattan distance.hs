points :: Int -> [(Int, Int)] 
points x = [(i, j) | i <- [0..x], j <- [0..x], i + j <= x]