myLast :: [a] -> a
myLast [] = error "empty list"
myLast [a] = a
myLast (_ : xs) = myLast xs

myLast' :: [a] -> a
myLast' = foldr1 (flip const)

-- 素晴らしい
myLast'' :: [a] -> a
myLast'' = foldr1 (const id)

myLast''' = head . reverse

myLast'''' :: [a] -> a
myLast'''' = foldl1 (curry snd)