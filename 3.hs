elementAt xs n = xs !! (n - 1)

elementAt' xs n 
  | length xs < n = error "Index out of bounds"
  | otherwise = fst . last $ zip xs [1..n] 

elementAt'' xs n = head $ foldr ($) xs 
                        $ replicate (n - 1) tail