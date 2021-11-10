module Lib where

import Data.List
import Data.Monoid

-- >>> test 3
-- 4

prop_law_Functor_id :: (Eq (f a), Functor f) => f a -> Bool
prop_law_Functor_id x = fmap id x == x

prop_law_Functor_comp :: (Functor f, Eq (f c)) => (b -> c) -> (a -> b) -> f a -> Bool
prop_law_Functor_comp g f x = fmap (g . f) x == (fmap g . fmap f) x

data List a = Nil | Cons a (List a) deriving (Show, Eq, Ord)

listMap :: (a -> b) -> List a -> List b
listMap _ Nil = Nil
listMap f (Cons x xs) = Cons (f x) (listMap f xs)

instance Functor List where
    fmap = listMap

-- prop_law_Functor_id x == True pour tout x

data Pair a b = Pair a b
    deriving (Show, Eq)

data PairM a = PairM a a
    deriving (Show, Eq)

instance Functor PairM where 
    fmap f (PairM x y) = PairM (f x) (f y)

-- >>> fmap (+1) (PairM 1 2)
-- PairM 2 3

instance Functor (Pair e) where
    -- fmap :: (a -> b) -> (Pair e a) -> (Pair e b)
    fmap f (Pair x y) = Pair x $ f y

-- >>> fmap (+1) (Pair 1 2) 
-- Pair 1 3

-- instance Functor ((->) e) where
--     fmap = arrowmap 

arrowMap :: (a -> b) -> (e -> a) -> (e -> b)
arrowMap = (.) 

-- prop_id ==> id . x == x
-- prop_comp ==> (g . f) . x == g . (f . x)

data NonEmptyList a = NECons a [a]

nel2l :: NonEmptyList a -> [a]
nel2l (NECons x xs) = x:xs

nelComp :: NonEmptyList a -> NonEmptyList a -> NonEmptyList a
nelComp (NECons x xs) (NECons y ys) = NECons x $ xs <> [y] <> ys 
-- nelComp (NECons x xs) (NECons y ys) = NECons x $ xs <> (y : ys) 
nelComp2 :: NonEmptyList a -> NonEmptyList a -> NonEmptyList a
nelComp2 (NECons x xs) l = NECons x (xs <> nel2l l)

instance Semigroup (NonEmptyList a) where
    (<>) = nelComp

-- x <> (y <> z) = (x <> y) <> z

newtype NatMax = NM Integer deriving (Eq, Show)

nmax :: Integer -> Maybe NatMax
nmax x | x >= 0 = Just $ NM x
       | otherwise = Nothing

instance Semigroup NatMax where
    (<>) (NM x) (NM y) = if x >= y then NM x else NM y

instance Monoid NatMax where
    mempty = NM 0

data Tree a = Tip | Node a (Tree a) (Tree a)
        deriving (Show, Eq)

exTree :: Tree Integer
exTree = Node 17 (Node 24 (Node 12 (Node 9 Tip Tip) Tip)
                (Node 42 Tip Tip))
                (Node 19 Tip (Node 11 Tip Tip))


treeFoldMap :: Monoid m => (a -> m) -> Tree a -> m
treeFoldMap f Tip = mempty 
treeFoldMap f (Node x ag ad) = treeFoldMap f ag <> f x <> treeFoldMap f ad

treeFoldMap2 :: Monoid m => (a -> m) -> Tree a -> m
treeFoldMap2 f Tip = mempty 
treeFoldMap2 f (Node x ag ad) = f x <> treeFoldMap f ag <> treeFoldMap f ad



-- >>> treeFoldMap NM exTree
-- NM 42
-- >>> treeFoldMap show exTree
-- "9122442171911"
-- >>> treeFoldMap Sum exTree
-- Sum {getSum = 134}

instance Foldable Tree where
    foldMap = treeFoldMap2

-- >>> foldMap (\x -> show x <> " ") exTree
-- "17 9 12 24 42 19 11 "

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

myFoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
myFoldMap f = foldr step mempty
    where step x acc = f x <> acc

-- >>> myFoldMap (\x -> show x <> " ") exTree
-- "17 9 12 24 42 19 11 "

newtype Endof a = E (a -> a)

instance Semigroup (Endof a) where
    E f <> E g = E $ f . g

instance Monoid (Endof a) where
    mempty = E id

appliqueE :: Endof a -> a -> a
appliqueE (E f) = f 

myFoldr :: (Foldable t) => (a -> b -> b) -> b -> t a -> b 
myFoldr step initial structure = appliqueE (foldMap (E . step) structure) initial

