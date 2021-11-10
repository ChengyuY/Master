module Revrev where

-- reverse :: [a] -> [a]

prop_revrev :: Eq a => [a] -> Bool
prop_revrev xs = reverse (reverse xs) == xs


-- >>> prop_revrev "abc"
-- True

prop_revapp :: Eq a => [a] -> [a] -> Bool
prop_revapp xs ys = 
    reverse (ys <> xs) == reverse xs <> reverse ys

-- >>>prop_revapp [1,2,3] [4,5,6]
-- True

                      
