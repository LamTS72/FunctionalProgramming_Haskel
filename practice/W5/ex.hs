pyths :: Int -> [(Int ,Int ,Int)]
pyths n = [(x,y,z)| x <-[1..n],y <-[1..n], z <-[1..n], x^2 +y^2 == z^2]

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)= if (x <= y) then x: (merge xs (y:ys)) else y: (merge ys (x:xs)) 

msort :: Ord a => [a] -> [a]
msort [] = []
msort (x:[]) = [x]
msort xs = merge ( msort (take (length xs `div` 2) xs)) (msort(drop (length xs `div` 2) xs))