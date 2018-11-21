myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' = foldl (\n _ -> n + 1) 0

myLength'' xs = snd. last $ zip xs [1..]

myLength''' :: [a] -> Int
myLength''' = sum . map (\_->1)