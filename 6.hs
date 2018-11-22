import Control.Applicative
import Control.Monad
import Control.Arrow

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = foldr (&&) True $ zipWith (==) xs (reverse xs) 

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' = liftM2 (==) id reverse

isPalindrome'' :: (Eq a) => [a] -> Bool
isPalindrome'' = (==) <*> reverse

isPalindrome''' :: (Eq a) => [a] -> Bool
isPalindrome''' = (uncurry (==) . (id &&& reverse))