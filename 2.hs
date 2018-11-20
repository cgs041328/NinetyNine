myButLast :: [a] -> a
myButLast = last . init

myButLast' [] = error "empty list"
myButLast' [x] = error "not enough elements"
myButLast' (x:(_:[])) = x
myButLast' (_:xs) = myButLast' xs

myButLast'' :: Foldable f => f a -> a
myButLast'' = fst. foldl (\(a, b) c -> (b, c)) (err1, err2)
    where err1 = error "empty list"
          err2 = error "not enough elements"