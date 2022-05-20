nLast ns = ns !! (length ns - 1)

nLLast ns = head (reverse ns)

nLLLast ns = take 1 (reverse ns)

nllLast ns = drop (length ns - 1) ns

nlLLast ns = (reverse ns) !! 0

iNit is = take (length is - 1) is

iNNit is = reverse (tail (reverse is))

inNit is = reverse (drop 1(reverse is))

