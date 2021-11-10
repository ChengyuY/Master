{-# LANGUAGE InstanceSigs #-}

module Lib where

essai :: Int -> Int 
essai = const 42

-- >>> essai 12
-- 42

newtype Lecteur e a = Lecteur {runLecteur :: e -> a}

instance Functor (Lecteur e) where
    fmap g (Lecteur f) = Lecteur (g . f)

instance Applicative (Lecteur e) where
    -- pure x = Lecteur (\_ -> x)
    pure = Lecteur . const
    (<*>) (Lecteur f) (Lecteur g) = Lecteur (\x -> f x (g x))

r1 :: Lecteur Int Int
r1 = (+1) <$> Lecteur (\x -> x - 1)

-- >>> :t r1
-- r1 :: Lecteur Int Int
-- >>> (runLecteur r1) 5
-- 5
-- >>> (runLecteur r1) 10
-- 10

r2 :: Lecteur Int Int
r2 = (+) <$> Lecteur (+3) <*> Lecteur (*100)
-- <Lecteur Int (Int -> Int)> | <Lecteur Int Int>

-- >>> (runLecteur r2) 5
-- 508
-- >>> (runLecteur r2) 10
-- 1013

data Paire a b = Paire a b deriving (Show, Eq)

instance Functor (Paire e) where
    fmap g (Paire x y) = Paire x (g y)

instance (Monoid e) => Applicative (Paire e) where
    pure x = Paire mempty x
    (<*>) (Paire e1 f) (Paire e2 x) = Paire (e1 <> e2) (f x)

-- >>> (+1) <$> (Paire "x" 41)
-- Paire "x" 42
-- >>>  (+) <$> (Paire "Hello" 39) <*> (Paire " world!" 3)
-- Paire "Hello world!" 42


-- >>>  (+) <$> [1, 2, 3] <*> [10, 20, 30]
-- [11,21,31,12,22,32,13,23,33]
-- >>> [(+), (*)] <*> [1, 2, 3] <*> [10, 20, 30]
-- [11,21,31,12,22,32,13,23,33,10,20,30,20,40,60,30,60,90]
-- >>> (+) <$> [1, 2, 3] <*> [10]
-- [11,12,13]


newtype ZList a = ZL [a] deriving (Eq, Show)

instance Functor ZList where
    fmap f (ZL l) = ZL (f <$> l)

-- >>> (+1) <$> (ZL [1, 2, 3])
-- ZL [2,3,4]

applyZL :: ZList (a -> b) -> ZList a -> ZList b
applyZL (ZL []) _ = ZL []
applyZL _ (ZL []) = ZL []
applyZL (ZL (f:fs)) (ZL (x:xs)) = let ZL fxs = applyZL (ZL fs) (ZL xs) in
                                    ZL (f x:fxs)

-- >>> applyZL ((+) <$> (ZL [1, 2, 3])) (ZL [10, 11])
-- ZL [11,13]
-- >>> applyZL ((+) <$> (ZL [1, 2, 3])) (ZL [10, 11, 12, 14, 120])
-- ZL [11,13,15]
-- >>> applyZL (ZL [(*2), (*3), (*4)]) (ZL [])
-- ZL []

pureZL :: a -> ZList a
pureZL = ZL . repeat

-- >>>  (pureZL id) `applyZL` (ZL [1, 2, 3, 4])
-- ZL [1,2,3,4]
-- >>> id <$> ZL [1, 2, 3, 4]
-- ZL [1,2,3,4]
-- >>> (pureZL id) `applyZL` ZL [1, 2, 3, 4]
-- ZL [1,2,3,4]

instance Applicative ZList where
    pure = pureZL
    (<*>) = applyZL

-- >>> (ZL [(+), (*)]) <*> (ZL [1, 2, 3]) <*> (ZL [10, 20, 30])
-- ZL [11,40]

-- *** Exercice 3 ***

data Arbre a = Bout | Noeud a (Arbre a) (Arbre a) deriving (Eq, Show)

ex1 :: Arbre Int 
ex1 = Noeud 17 (Noeud 24 (Noeud 12 (Noeud 9 Bout Bout) Bout)
        (Noeud 42 Bout Bout))
        (Noeud 19 Bout (Noeud 11 Bout Bout))

-- >>> ex1
-- Noeud 17 (Noeud 24 (Noeud 12 (Noeud 9 Bout Bout) Bout) (Noeud 42 Bout Bout)) (Noeud 19 Bout (Noeud 11 Bout Bout))

ex2 :: Arbre Integer
ex2 = Noeud 18 (Noeud 24 (Noeud 12 (Noeud 8 Bout Bout) Bout)
        (Noeud 42 Bout Bout))
        (Noeud 20 Bout (Noeud 12 Bout Bout))

-- >>> ex2
-- Noeud 18 (Noeud 24 (Noeud 12 (Noeud 8 Bout Bout) Bout) (Noeud 42 Bout Bout)) (Noeud 20 Bout (Noeud 12 Bout Bout))

arbreFoldMap :: Monoid m => (a -> m) -> Arbre a -> m
arbreFoldMap _ Bout = mempty 
arbreFoldMap f (Noeud v g d) = arbreFoldMap f g <> f v <> arbreFoldMap f d

instance Foldable Arbre where
    foldMap = arbreFoldMap


instance Functor Arbre where
    fmap f Bout = Bout
    fmap f (Noeud v g d) = Noeud (f v) (fmap f g) (fmap f d)

-- >>> (+1) <$> ex1
-- Noeud 18 (Noeud 25 (Noeud 13 (Noeud 10 Bout Bout) Bout) (Noeud 43 Bout Bout)) (Noeud 20 Bout (Noeud 12 Bout Bout))

traverseArbre :: Applicative f => (a -> f b) -> Arbre a -> f (Arbre b)
traverseArbre phi Bout = pure Bout
traverseArbre phi (Noeud v g d) = Noeud <$> phi v <*> traverseArbre phi g <*> traverseArbre phi d

instance Traversable Arbre where
    traverse = traverseArbre

pairArbre :: Integral a => Arbre a -> Maybe (Arbre a)
pairArbre = traverse (\x -> if even x then Just x else Nothing)

-- >>> pairArbre ex1
-- Nothing
-- >>> pairArbre ex2
-- Just (Noeud 18 (Noeud 24 (Noeud 12 (Noeud 8 Bout Bout) Bout) (Noeud 42 Bout Bout)) (Noeud 20 Bout (Noeud 12 Bout Bout)))

-- *** Exercice 4 ***


type Erreur = String 

verifPair :: (Integral a, Show a) => a -> Either [Erreur] a
verifPair n | even n = Right n
            | otherwise = Left ["Pas pair : " <> show n]

-- >>> verifPair 8
-- Right 8
-- >>> verifPair 7
-- Left ["Pas pair : 7"]

-- >>> (+) <$> (verifPair 8) <*> (verifPair 6)
-- Right 14
-- >>> (+) <$> (verifPair 8) <*> (verifPair 7)
-- Left ["Pas pair : 7"]
-- >>> (+) <$> (verifPair 7) <*> (verifPair 6)
-- Left ["Pas pair : 7"]
-- >>> (+) <$> (verifPair 7) <*> (verifPair 11)
-- Left ["Pas pair : 7"]

newtype Validation e a = Val (Either e a) deriving (Eq, Show)

instance Functor (Validation e) where
    fmap f (Val ei) = Val (fmap f ei) 

-- >>> (+1) <$> (Val $ Left ["erreur"])
-- Val (Left ["erreur"])
-- >>> (+1) <$> (Val $ Right 3) :: (Validation [String] Int)
-- Val (Right 4)

instance Monoid e => Applicative (Validation e) where
    pure = Val . Right
    (<*>) = applyVal

applyVal :: (Monoid e) => Validation e (a -> b) -> Validation e a -> Validation e b
applyVal (Val (Left er)) (Val (Right x)) =  Val (Left er)
applyVal (Val (Right f)) (Val (Left er)) = Val (Left er)
applyVal (Val (Right f)) (Val (Right x)) = Val (Right (f x))
applyVal (Val (Left er1)) (Val (Left er2)) = Val (Left (er1 <> er2))

type Valid a = Validation [Erreur] a

valide :: a -> Valid a
valide = pure

invalide :: String -> Valid a
invalide = Val . Left . (: [])

validePair :: (Integral a, Show a) => a -> Valid a
validePair n | even n = valide n
             | otherwise = invalide $ "Ce n'est pas un nombre pair: " <> (show n)


-- >>> (,) <$> (validePair 42) <*> (validePair 38)
-- Val (Right (42,38))
-- >>> (+) <$> (validePair 42) <*> (validePair 38)
-- Val (Right 80)
-- >>> (+) <$> (validePair 43) <*> (validePair 38)
-- Val (Left ["Ce n'est pas un nombre pair: 43"])
-- >>> (+) <$> (validePair 42) <*> (validePair 39)
-- Val (Left ["Ce n'est pas un nombre pair: 39"])
-- >>> (+) <$> (validePair 43) <*> (validePair 39)
-- Val (Left ["Ce n'est pas un nombre pair: 43","Ce n'est pas un nombre pair: 39"])

valideNonVide :: String -> Valid ()
valideNonVide [] = invalide "La chaine est vide"
valideNonVide xs = valide ()

valideMinuscule :: Char -> Valid Char
valideMinuscule ch
            | (ch >= 'a' && ch <= 'z') = valide ch
            | otherwise = invalide ("Ce n'est pas une miniscule : " <> show ch)

valideMajuscule :: Char -> Valid Char
valideMajuscule ch
    | (ch >= 'A' && ch <= 'Z') = valide ch
    | otherwise = invalide ("Ce n'est pas une majuscule : " <> (show ch))


valideNom :: String -> Valid String
valideNom str = (:) <$> (vNE *> vH) <*> vT
    where vNE = valideNonVide str
          vH = case str of
              [] -> Val $ Right 'X'
              c:cs -> valideMajuscule c
          vT = case str of
              [] -> Val $ Right []
              c:cs -> traverse valideMinuscule cs

-- >>> valideNom ""
-- Val (Left ["La chaine est vide"])
-- >>> valideNom "romain"
-- Val (Left ["Ce n'est pas une majuscule : 'r'"])
-- >>> valideNom "ROmain"
-- Val (Left ["Ce n'est pas une miniscule : 'O'"])
-- >>> valideNom "rOmAin"
-- Val (Left ["Ce n'est pas une majuscule : 'r'","Ce n'est pas une miniscule : 'O'","Ce n'est pas une miniscule : 'A'"])
-- >>> valideNom "Romain"
-- Val (Right "Romain")

valideNom2 :: String -> Valid String
valideNom2 str = (:) <$> (vNE *> vH) <*> vT
    where vNE = valideNonVide str
          vH = valideMajuscule (head str)
          vT = case str of
              [] -> Val $ Right []
              c:cs -> traverse valideMinuscule cs

-- >>> valideNom2 "Romain"
-- Val (Right "Romain")
-- >>> valideNom2 "romain"
-- Val (Left ["Ce n'est pas une majuscule : 'r'"])

-- >>> (+) <$> [1, 2, 3] *> [4, 5, 6]
-- [4,5,6,4,5,6,4,5,6]
