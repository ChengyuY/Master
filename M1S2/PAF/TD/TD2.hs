module Lib where

-- >>> head ["bonjour", "monde", "!"]
-- "bonjour"
-- >>> head [1]
-- 1
-- >>> head (tail [1])
-- Prelude.head: empty list
-- >>> head []
-- Ambiguous type variable ‘f0’ 
-- prevents the constraint ‘(Show f0)’ from being solved.
-- Probable fix: use a type annotation to specify what ‘f0’ should be.
-- These potential instances exist:
--   instance Show a => Show (ZipList a)
--     -- Defined in ‘Control.Applicative’
--   instance Show NestedAtomically
--     -- Defined in ‘Control.Exception.Base’
--   instance Show NoMethodError -- Defined in ‘Control.Exception.Base’
--   ...plus 201 others
--   (use -fprint-potential-instances to see them all)

headPlus :: [Integer] -> Integer -> Integer 
headPlus xs n = head xs + n

-- >>> headPlus [1, 2, 3] 4 
-- 5
-- >>> headPlus [] 4
-- Prelude.head: empty list

safeHead :: [t] -> Maybe t
safeHead [] = Nothing 
safeHead (x:xs) = Just x

-- >>> safeHead [1, 2, 3]
-- Just 1
-- >>> safeHead (tail [1])
-- Nothing

safeHeadPlus :: [Integer] -> Integer -> Maybe Integer 
safeHeadPlus xs n = case safeHead xs of
    Nothing -> Nothing
    Just m -> Just $ m + n

data EmptyList = EmptyList deriving Show

eitherHead :: [a] -> Either EmptyList a
eitherHead [] = Left EmptyList
eitherHead (x:xs) = Right x

-- >>> eitherHead [1, 2, 3]
-- Right 1
-- >>> eitherHead (tail [1])
-- Left EmptyList

eitherHeadPlus :: [Integer] -> Integer -> Either EmptyList Integer
eitherHeadPlus xs n = case eitherHead xs of
    Left _ -> Left EmptyList
    Right m -> Right $ m + n

-- >>> eitherHeadPlus [] 3
-- Left EmptyList
-- >>> eitherHeadPlus [1, 2, 3] 3
-- Right 4

defaultHeadInteger :: [Integer] -> Integer 
defaultHeadInteger [] = 0
defaultHeadInteger (x:xs) = x

defaultHead :: [a] -> a -> a
defaultHead [] y = y
defaultHead (x:xs) y = x

defaultHeadPlus :: [Integer] -> Integer -> Integer 
defaultHeadPlus xs n = defaultHead xs 0 + n
-- >>> defaultHeadPlus [1, 2, 3] 3
-- 4
-- >>> defaultHeadPlus [] 3
-- 3

data List a =
    Nil
    | Cons a (List a)
    deriving (Show, Eq)

listMap :: (a -> b) -> List a -> List b
listMap _ Nil = Nil
listMap f (Cons e l) = Cons (f e) (listMap f l)

-- >>> :t foldl
-- foldl
--   :: forall (t :: * -> *) b a.
--      Foldable t =>
--      (b -> a -> b) -> b -> t a -> b

-- >>> foldl (\x _ -> x + 1) 0 $ Cons 1 $ Cons 2 $ Cons 3 Nil 
-- >>> foldl (\x y -> x ++ y) "" ["a", "b", "c"]
-- No instance for (Foldable List) arising from a use of ‘foldl’
-- "abc"

listFoldl :: (b -> a -> b) -> b -> List a -> b
listFoldl f acc Nil = acc
listFoldl f acc (Cons x xs) = listFoldl f (f acc x) xs

listFoldr :: (b -> a -> b) -> b -> List a -> b
listFoldr f acc Nil = acc
listFoldr f acc (Cons x xs) = f (listFoldr f acc xs) x

-- >>> listFoldl (\x _ -> x + 1) 0 $ Cons 1 $ Cons 2 $ Cons 3 Nil
-- 3
-- >>> listFoldl (\x y -> x + y) 0 $ Cons 1 $ Cons 2 $ Cons 3 Nil
-- 6
-- >>> listFoldl (\x y -> x ++ y) "" $ Cons "a" $ Cons "b" $ Cons "c" Nil
-- "abc"

listMap2 :: (a -> b) -> List a -> List b
listMap2 f = listFoldr aux Nil
    where aux ys z = Cons (f z) ys

-- >>> listMap2 (\x -> x + 3) $ Cons 1 $ Cons 2 $ Cons 3 Nil
-- Cons 4 (Cons 5 (Cons 6 Nil))

prop_lmap :: Eq b => (a -> b) -> List a -> Bool 
prop_lmap f xs = listMap f xs == listMap2 f xs
