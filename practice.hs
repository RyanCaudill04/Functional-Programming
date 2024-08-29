myreverse :: [a] -> [a]
myreverse (x:xs) = myreverse xs ++ [x]
