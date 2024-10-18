clusters :: (String -> String -> Float) -> Float -> [String] -> [[String]]
clusters _ _ [] = []  
clusters f d (x:xs) = clusterHelper f d xs [[x]] 
clusterHelper :: (String -> String -> Float) -> Float -> [String] -> [[String]] -> [[String]]
clusterHelper _ _ [] clusters = clusters  
clusterHelper f d (s:ss) clusters = clusterHelper f d ss (updatedClusters ++ newCluster)
  where
    (closeCluster, remainingCluster) = partition (\c -> any (\str -> f str s <= d) c) clusters
    updatedClusters = map (\c -> if any (\str -> f str s <= d) c then s:c else c) closeCluster
    newCluster = if any (\str -> f str s <= d) (concat closeCluster) then [] else [[s]]
