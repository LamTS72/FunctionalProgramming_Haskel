-- safetail
-- a conditional expression

safetail xs = if xs == [] then [] else tail xs

-- guarded equations

safetail' xs |xs == [] = []
	     |otherwise = tail xs

-- pattern matching

safetail'' [] = []
safetail'' (_:xs) = xs
