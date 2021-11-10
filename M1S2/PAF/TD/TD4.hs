module Lib
     where

uns :: [Integer]
uns = 1 : uns

-- >>> take 20 uns
-- [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]

genInteger :: Integer -> [Integer]
genInteger n = n : genInteger n

-- >>> take 100 $ genInteger 42
-- [42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42]
-- >>> take 100 $ repeat "oui"
-- ["oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui"]

prend :: Int -> [a] -> [a]
prend _ [] = []
prend n (x:xs)
     | n > 0 = x : prend (n-1) xs
     | otherwise = []
-- >>> prend 10 $ genInteger 42
-- >>> prend (10 * 10) $ repeat "oui"
-- [42,42,42,42,42,42,42,42,42,42]
-- ["oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui","oui"]

-- inductive, stricte en son 1er argument, non-stricte en son 2eme, totale

genNats :: Int -> [Int]
genNats n = n : genNats (n + 1)

-- >>> prend 100 $ genNats 42
-- [42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141]

nats :: [Int]
nats = genNats 0

-- >>> prend 20 nats
-- [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]
-- >>> prend (-1) uns
-- []


nats2 :: [Int]
nats2 = 0 : map (+ 1) nats2

-- >>> prend 10 nats2
-- [0,1,2,3,4,5,6,7,8,9]

genFibo :: Int -> Int -> [Int]
genFibo m f = m : genFibo f (m + f)

fibo :: [Int]
fibo = genFibo 0 1

-- >>> prend 20 fibo
-- [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181]

fibo2 :: [Int]
fibo2 = 0 : 1 : zipWith (+) fibo2 (tail fibo2)

-- >>> prend 20 fibo2
-- [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181]

-- >>> [x * x | x <- [1..5]]
-- [1,4,9,16,25]

-- >>> [x * y | x <- [1..3], y <- [1..3]]
-- [1,2,3,2,4,6,3,6,9]

-- >>> prend 20 [x * x | x <- fibo2, even x]
-- [0,4,64,1156,20736,372100,6677056,119814916,2149991424,38580030724,692290561600,12422650078084,222915410843904,4000054745112196,71778070001175616,1288005205276048900,4665571551258152960,8905306422532497924,8055991464650396736,6975331425207782020]

pMap :: (a -> [b]) -> [a] -> [b]
pMap _ [] = []
pMap f (x:xs) = f x <> pMap f xs

-- >>> pMap (\n -> [x | x <- [1..n]]) [1, 2, 3]
-- [1,1,2,1,2,3]
-- >>> pMap (\x -> [x, 10 * x, 100 * x]) [1, 2, 3, 4, 5]
-- [1,10,100,2,20,200,3,30,300,4,40,400,5,50,500]

compr :: [a] -> (a -> [b]) -> [b]
compr = flip pMap

-- >>> [x * x | x <- [1..5]]
-- [1,4,9,16,25]
-- >>> compr [1..5] (\x -> [x * x])
-- [1,4,9,16,25]

-- >>> take 10 ([1..] `compr` (\x -> [x * x]))
-- [1,4,9,16,25,36,49,64,81,100]

succes :: a -> [a]
succes x = [x]

echec :: [a]
echec = []

-- >>>  [1..8] `compr` (\x -> if even x then (succes x) else echec)
-- [2,4,6,8]

select :: (a -> Bool) -> (a -> b) -> a -> [b]
select p f x = if p x then succes (f x) else echec  

-- >>> [1..8] `compr` (select even id)
-- [2,4,6,8]
-- >>> take 10 ([1..] `compr` (\x -> if even x then (succes x) else echec))
-- [2,4,6,8,10,12,14,16,18,20]

-- >>> take 10 ([1..] `compr` (select even id))
-- [2,4,6,8,10,12,14,16,18,20]

machin :: [(Int, Int)]
machin = take 10 $ [1..] `compr` (\x -> [1..4] `compr` select (\y -> x <= y) (\y -> (x,y)) )

-- >>> machin
-- [(1,1),(1,2),(1,3),(1,4),(2,2),(2,3),(2,4),(3,3),(3,4),(4,4)]

-- >>> :t id
-- id :: forall a. a -> a

data Arbre a =
          Bout
          | Noeud a (Arbre a) (Arbre a)
     deriving Show

feuille :: a -> Arbre a
feuille v = Noeud v Bout Bout

exArbre :: Arbre Int
exArbre = Noeud 5 (Noeud 7 (feuille 6) (feuille 4))
     (Noeud 2 (feuille 1) (feuille 3))

maxArbre :: (Ord a) => Arbre a -> a
maxArbre Bout = error "Arbre vide : pas de maximum"
maxArbre (Noeud n Bout Bout) = n
maxArbre (Noeud n t Bout) = max n $ maxArbre t
maxArbre (Noeud n Bout t) = max n $ maxArbre t
maxArbre (Noeud n g d) = max n (max (maxArbre g) (maxArbre d))

-- >>> maxArbre exArbre
-- 7
-- >>> maxArbre $ Noeud 3 (feuille 10) (feuille 14)
-- 14

addArbre :: Int -> Arbre Int -> Arbre Int 
addArbre _ Bout = Bout
addArbre x (Noeud n g d) = Noeud (x + n) (addArbre x g) (addArbre x d)

-- >>> addArbre 3 exArbre
-- Noeud 8 (Noeud 10 (Noeud 9 Bout Bout) (Noeud 7 Bout Bout)) (Noeud 5 (Noeud 4 Bout Bout) (Noeud 6 Bout Bout))

addMaxArbre :: Arbre Int -> Arbre Int 
addMaxArbre t = t' 
     where (t', mx) = traversante t
           traversante Bout = error "Arbre vide : pas de maximum"
           traversante (Noeud n Bout Bout) = (Noeud (mx + n) Bout Bout, n)
           traversante (Noeud n t Bout) = (Noeud (mx + n) t2 Bout, max n m2)
               where (t2, m2) = traversante t
           traversante (Noeud n Bout t) = (Noeud (mx + n) Bout t3, max n m3)
               where (t3, m3) = traversante t
           traversante (Noeud n g d) = (Noeud (mx + n) t4 t5, max n (max m4 m5))
               where (t4, m4) = traversante g
                     (t5, m5) = traversante d


-- >>> addMaxArbre exArbre
-- Noeud 12 (Noeud 14 (Noeud 13 Bout Bout) (Noeud 11 Bout Bout)) (Noeud 9 (Noeud 8 Bout Bout) (Noeud 10 Bout Bout))
