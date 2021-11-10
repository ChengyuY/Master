module Lib
    ( someFunc
    ) where


someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- >>> Just(42 :: Integer)
-- Just 42

-- >>> Just (42)
-- Just 42

data AdresseLivraison = 
    AdresseLivraison{numVoie::Integer,nomVoie :: String,digicode::Maybe String}

fact::Integer ->Maybe Integer 
fact 0 = Just 1
fact n 
    |n>0 = case fact(n-1) of
            Nothing -> Nothing 
            Just k -> Just $ n * k --ou Just (n*k)
    |otherwise = Nothing 

-- >>> fact(-1)
-- Nothing
-- >>> fact(5)
-- Just 120

-- >>> :k Integer
-- Integer :: *

-- >>> :k (Maybe Integer)
-- (Maybe Integer) :: *

data MyEither a b = 
    MyLeft a
    | MyRight b
    deriving(Show,Eq)



-- >>> :t MyLeft (42 :: Integer)
-- MyLeft (42 :: Integer) :: forall b. MyEither Integer b
-- >>> :t MyLeft "Ceci est une err"
-- MyLeft "Ceci est une err" :: forall b. MyEither [Char] b

-- >>> :k Either
-- Either :: * -> * -> *



-- >>> :k Either String
-- Either String :: * -> *

-- >>> :k Either String Bool
-- Either String Bool :: *

-- >>> Left "error" :: Either String Bool
-- Left "error"


-- >>> quot 3 0
-- divide by zero
-- le programme s'arret

safeQuot :: Integer -> Integer -> Either String Integer
safeQuot _ 0 = Left "division par zero"
safeQuot n m = Right (quot n m)

-- >>> safeQuot 42 3
-- Right 14

-- >>> safeQuot 3 0
-- Left "division par zero"


data PNat = 
    Z -- zéro
    | S PNat --successeur
    deriving (Show,Eq) -- par défaut, légalité structurelle

one :: PNat
one = S Z

two :: PNat
two = S(S Z)

three :: PNat
three = S (S (S Z)) -- S $ S $ S Z

padd :: PNat -> PNat -> PNat
padd Z m = m --cas de base
padd (S n) m = S(padd n m) --cas résursif

-- >>> padd two three
-- S (S (S (S (S Z))))

-- >>> padd two one
-- S (S (S Z))

-- >>> (one `padd` two) == three
-- True

prop1 :: PNat -> Bool 
prop1 n = padd n Z == n

-- >>> prop1 Z
-- True


-- >>> prop1 (S Z)
-- True

data List a = Nil
            | Cons a (List a)
            deriving(Show,Eq)

-- >>>:t Cons 1 (Cons 2 (Cons 3 Nil))
-- Cons 1 (Cons 2 (Cons 3 Nil)) :: forall a. Num a => List a

-- >>>:t Cons (1::Integer) (Cons 2 (Cons 3 Nil))
-- Cons (1::Integer) (Cons 2 (Cons 3 Nil)) :: List Integer

-- >>> :t []
-- [] :: forall a. [a]

-- >>> 1:2:3:[]
-- [1,2,3]

-- >>> :t [1,2,3,4]
-- [1,2,3,4] :: forall a. Num a => [a]

-- >>> :t [1,2,3,4::Integer]
-- [1,2,3,4::Integer] :: [Integer]

--specifique aux liste
-- >>> [1,2,3,4] ++ [3,4,5]
-- [1,2,3,4,3,4,5]
--generique aux monoides
-- >>> [1,2,3,4] <> [3,4,5] 
-- [1,2,3,4,3,4,5]


plength :: List a -> PNat
plength Nil = Z
plength (Cons _ xs) = S (plength xs)

-- >>> plength (Cons "un" (Cons "deux" (Cons "trois" Nil)))
-- S (S (S Z))

longueur :: [a] -> Integer
longueur [] = 0
longueur (_ :xs) = 1 +(longueur xs)

-- >>> longueur ["un","deux","trois"]
-- 3


myMap :: (a->b) -> List a -> List b
myMap f Nil = Nil
myMap f (Cons x xs) = Cons (f x) (myMap f xs)

-- >>> myMap (\x -> x + 1) (Cons 0 (Cons 1 (Cons 2 Nil)))
-- Cons 1 (Cons 2 (Cons 3 Nil))

-- >>> fmap (\x -> x + 1) [0,1,2]
-- [1,2,3]

-- >>> fmap (+1) [0,1,2]
-- [1,2,3]

comp :: (b -> c) -> (a -> b) -> a -> c
comp g f = \x -> g (f x)



-- >>> :t 42
-- 42 :: forall p. Num p => p

-- >>> 42 :: Integer
-- 42




data Tank = Tank Integer Integer 
    | FullTank Integer 
    | EmptyTank Integer 
    deriving(Show)

capacity :: Tank -> Integer
capacity (Tank _ cap) = cap
capacity (FullTank cap) = cap
capacity (EmptyTank cap) = cap

quantity :: Tank -> Integer 
quantity (Tank qty _ ) = qty
quantity (FullTank cap) = cap
quantity (EmptyTank cap) = 0


