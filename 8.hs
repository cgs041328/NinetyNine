import Data.List

compress :: Eq a => [a] -> [a]
compress = map head . group

compress' :: Eq a => [a] -> [a]
compress' (x:ys@(y:_)) 
    | x == y = compress' ys
    | otherwise = x : compress' ys
compress' xs = xs

compress'' []     = []
compress'' (x:xs) = x : (compress'' $ dropWhile (== x) xs)