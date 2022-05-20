doubleAll :: Num a => [a] -> [a] 
doubleAll [] = []
doubleAll xs = [2 * head xs ] ++ doubleAll(tail xs)

removeIndex :: Int -> [a] -> [a]
removeIndex x ns = (take x ns) ++ (drop (x+1) ns)