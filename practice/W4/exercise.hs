searchL :: Int -> [Int] -> Int 
searchL x (a:l) | x == a = 0
                | otherwise = (1+(searchL x l))


positives :: [Int] -> [Int]
positives (x:l) | x > 0 = ([x] ++ positives l)
                | otherwise = (positives l)