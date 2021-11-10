
module PAF_TranscriptCours2_2021 where

{-

Cours 2 : Type-driven design 2 : types paramétrés, types inductifs
==================================================================

-}


{-

Rappels (sic!) du cours 1 :  types "sommes de produits"
-------------------------

---------
Disclaimer : PAF n'est pas un cours *de* Haskell mais un cours *en* Haskell
Il faut au moins en partie apprendre ce langage par vous même, principalement
ce qui recouvre les prérequis Ocaml et la programmation fonctionnelle de base

==>  Learn you a Haskell for Great Good
     Miran Lipovaca -- Nostarch press (2011)
     cf. http://learnyouahaskell.com/chapters
----------

Reprenons ...

Au cours 1, on aurait du faire l'exemple suivant :

-}

data MoyenPaiement =
  Cheque Int  -- numéro du chèque
  | Carte String String -- nom du porteur, numéro de CB
  | Espece
  deriving (Show, Eq)

{-

Il s'agit d'un type "somme de produits" qui sera, avec les
fonctions associées, notre outil principal pour l'étape
de modélisation (conception) du logiciel.

Une bonne discussion de la "puissance" de cet outil :

     Domain Modeling Made Functional
     Scott Wlaschin -- Pragmatic Programmers (2018)
     https://fsharpforfunandprofit.com/books/

**Remarque** : les records (enregistrements) sont des "produits nommés"...
et en Haskell c'est même du sucre syntaxique pour les "vrais" produits.

Exemple de fonction prenant un tel type en entrée :

-}

description :: MoyenPaiement -> String
description mp = "Paiement par " <> (describe mp)
  where describe (Cheque n) = "Cheque No" <> (show n)
        describe (Carte nom num) = "Carte No" <> num <> ", nom : " <> nom
        describe Espece = "Espèce"

-- >>> description (Carte "toto" "12345")
-- "Paiement par Carte No12345, nom : toto"

{-

Les types représentent la structure des données, et non l'intégralité
de leur sémantique.

Dans notre cours, nous allons nous appuyer sur la notion de propriété
pour "ajouter" de la sémantique au modèle.

Voici une propriété portant sur le `MoyenPaiement` :

-}

prop_cartePorteurOk :: MoyenPaiement -> Bool
prop_cartePorteurOk (Carte nom _) = nom /= ""
prop_cartePorteurOk _ = True

-- Puisque cette propriété porte sur le type lui-même, et qu'elle
-- doit tout le temps être vérifiée, on dira qu'il s'agit
-- d'un **invariant** (de type)

{-

Nous verrons également durant PAF (principalement en TD) en quoi la programmation
fonctionnelle facilite également le raisonnement sur les programmes
  - raisonnement inductif ou par cas sur les types
  - raisonnement équationnel avec les fonctions

-}


{-

Passons maintenant au cours 2... Nous allons aborder deux extensions classiques
de types sommes-de-produits : les types paramétrés et les types inductifs/récursifs.

-}

{-

Les types paramétrés
--------------------

Objectif : permettre la généricité avec le polymorphisme paramétrique.

Exemple classique : le type option / Maybe (en Haskell)

-}

data MyMaybe a =
  MyNothing
  | MyJust a
  deriving (Show, Eq)

-- Remarque : les paramètres de type sont des variables (de type) et donc en minuscule

-- Terminologie :
--   MyMaybe est un *constructeur de type*  (*type constructor*) à un argument
--   par exemple : MyMaybe Integer    est un type.

--   MyNothing  est un *constructeur de données* (*data constructor*) à zéro argument
--   MyJust  est un *constructeur de données* (*data constructor*) à un argument

-- >>> MyNothing
-- MyNothing

-- >>> :t MyNothing
-- MyNothing :: MyMaybe a

-- traduction : MyNothing est un MyMaybe a  pour tout type a (Integer, Bool, String, etc.)

-- >>> :t MyJust True
-- MyJust True :: MyMaybe Bool

-- traduction : on a construit une donnée du type MyMaybe Bool

-- >>> Just (42 :: Integer)
-- Just 42

-- >>> :t Just (42 :: Integer)
-- Just (42 :: Integer) :: Maybe Integer

-- En haskell, le type officiel s'appelle   Maybe a   et les constructeurs Nothing et Just.

-- Rôles de ce type :

--  1) permettre de modéliser des  données optionnelles

data AdresseLivraison =
  AdresseLivraison { numVoie :: Int
                   , nomVoie :: String
                   , digicode :: Maybe String
                   }

-- 2) permettre d'implémenter des fonctions "simplement" partielles

fact :: Integer -> Maybe Integer
fact 0 = Just 1
fact n
  | n > 0 = case fact (n - 1) of
              Nothing -> Nothing
              Just k -> Just $ n * k   -- ou  Just (n * k)
  | otherwise = Nothing

-- >>> fact 4
-- Just 24

-- >>> fact (-1)
-- Nothing

{-

Interlude : types paramétrées et les kinds
---------

Un type paramétré, comme (Maybe a), est un constructeur de type (ici à 1 argument,
la variable) peut être vu comme une sorte de "fonction de type" qui attend un argument
(qui doit être un type).

Par exemple :  Maybe Int      est un type,    Maybe String  aussi, etc.

Question : quel est le "type" de   Maybe   "tout court" ?

Réponse :  le "type" de Maybe  est  * -> *  donc une "fonction de type"
  - qui prend un type en argument (repéré par l'étoile *)
  - et retourne un type en retour (idem)

-}

-- >>> :k Maybe
-- Maybe :: * -> *

-- >>> :k MyMaybe
-- MyMaybe :: * -> *

-- Les types ont tous le kind *

-- >>> :k Integer
-- Integer :: *

-- >>> :k (Maybe Integer)
-- (Maybe Integer) :: *

{-

La règle que l'on a utilisé :

F :: * -> *     T :: *
----------------------
    (F T) :: *


Cela ressemble à la règle de typage des applications :

f :: T -> T    e :: T
---------------------
    (f e) :: T

Le point important :  les types ont tous le même kind *   et les constructeurs
sont des "fonctions" de kind * -> *   (ou  * -> * -> *   à deux arguments, etc.)

-}

{-

Un deuxième exemple de type paramétré à deux arguments : Either a b
(prédéfini)

-}

data MyEither a b =
  MyLeft a
  | MyRight b
  deriving (Show, Eq)

-- >>> MyLeft "Ceci est une erreur"
-- MyLeft "Ceci est une erreur"

-- >>> :t MyLeft "Ceci est une erreur"
-- MyLeft "Ceci est une erreur" :: MyEither String b

-- >>> :t MyRight (42 :: Integer)
-- MyRight (42 :: Integer) :: MyEither a Integer

-- >>> :t Left "erreur"
-- Left "erreur" :: Either String b

-- >>> :t Right (42 :: Integer)
-- Right (42 :: Integer) :: Either a Integer

-- Question : quel est le kind de Either ?

-- >>> :k Either
-- Either :: * -> * -> *

-- >>> :k Either String
-- Either String :: * -> *

-- >>> :k Either String Bool
-- Either String Bool :: *

-- >>> Left "error"  :: Either String Bool
-- Left "error"

-- Exemple d'utilisation du Either comme "Maybe avec cas d'erreur" :

-- >>> quot 42 3
-- 14

-- >>> quot 3 0
-- *** Exception: divide by zero
--   (le programme s'est arrêté !)

-- >>> :doc div
--  integer division truncated toward negative infinity

-- >>> :doc quot
--  integer division truncated toward zero

safeQuot :: Integer -> Integer -> Either String Integer
safeQuot _ 0 = Left "division par zero"
safeQuot n m = Right (quot n m)

-- >>> safeQuot 42 3
-- Right 14

-- >>> safeQuot 3 0
-- Left "division par zero"

{-

Les types inductifs
-------------------

Coder des structures récursives (listes, arbres) ou co-récursives (streams, arbres infinis)...

Exemple "théorique" : les entiers de Peano

Un entier de Peano c'est Zéro ou le successeur d'un entier Peano

-}

data PNat =
  Z -- zéro
  | S PNat  -- successeur
  deriving (Show, Eq)  -- par défault, l'égalité structurelle

-- codage formellement parfait mais pratiquement très inefficace

-- par exemple, l'entier 3 :

one :: PNat
one = S Z

two :: PNat
two = S (S Z)

three :: PNat
three = S (S (S Z))  -- S $ S $ S Z

-- Premier intérêt de ce type de codage : les fonctions structurellement récursives

padd :: PNat -> PNat -> PNat
padd Z m = m                 -- cas de base
padd (S n) m = S (padd n m)  -- cas récursif

-- >>> padd two three
-- S (S (S (S (S Z))))

-- >>> two `padd` three
-- S (S (S (S (S Z))))

-- >>> (one `padd` two) == three
-- True

-- Exercice : multiplication, prédécesseur, puissance, division, etc.

-- Second intérêt : permettre les raisonnements inductifs

-- Pour chaque type inductif (paramétré ou non), on peut expliciter
-- un principe d'induction pour prouver des propriétés sur les programmes
-- (fonctions) qui manipulent ces types.

-- Le principe d'induction pour PNat est le suivant :

-- forall prop :: PNat -> Bool,
--    prop Z == True  /\  (forall n :: PNat, prop n == True  ==>  prop (S n) = True)
--    ==> forall n :: Pnat, prop n == True

-- Ce principe peut se déduire *automatiquement* à partir de la définition du type

{-
data PNat =
  Z -- zéro
  | S PNat  -- successeur
-}

-- Exemple de propriété :

prop1 :: PNat -> Bool
prop1 n = padd n Z == n


-- Une preuve une en mode semi-formel 

-- Cas de base pour Zero

-- prop1 Z
-- = padd Z Z == Z { equation de prop1 }
-- = Z == Z        { première équation de padd }
-- = True          { propriété de l'égalité structurelle }

-- Une autre façon de faire la preuve plus formellement :

-- >>> prop1 Z
-- True

-- Cas récursif
-- on considère un entier n :: PNat   quelconque
-- on suppose   prop1 n == True     (hypothèse d'induction)
-- ce qui veut dire:  padd n Z == n


-- on doit montrer    prop1 (S n) == True

-- prop1 (S n)
-- = padd (S n) Z  == S n   { équation de prop1 }
-- = S (padd n Z)  == S n   { seconde équation de padd }
-- = S n == S n             { par hypothèse d'induction }
-- = True                   { propriété de l'égalité structurelle }


{-

Exemple de type inductif paramétré : les listes

-}

data List a =
  Nil
  | Cons a (List a)
  deriving (Show, Eq)

-- >>> :t Cons 1 (Cons 2 (Cons 3 Nil))
-- Cons 1 (Cons 2 (Cons 3 Nil)) :: Num a => List a

-- >>> :t Cons (1 :: Integer) (Cons 2 (Cons 3 Nil))
-- Cons (1 :: Integer) (Cons 2 (Cons 3 Nil)) :: List Integer

-- En haskell, le type "List a"   s'appelle en fait  [a]  (à lire "list de a")

-- la liste vide est []

-- >>> :t []
-- [] :: [a]

-- >>> 1 : 2 : 3 : []
-- [1,2,3]

-- Raccourci syntaxique :   [e1, e2, ..., eN] pour construire une liste explicitement

-- >>> :t [1, 2, 3, 4]
-- [1, 2, 3, 4] :: Num a => [a]


-- >>> :t [1, 2, 3, 4 :: Integer]
-- [1, 2, 3, 4 :: Integer] :: [Integer]

-- >>> [1, 2, 3, 4] ++  [3, 4, 5]    -- spécifique aux listes
-- [1,2,3,4,3,4,5]

-- >>> [1, 2, 3, 4] <>  [3, 4, 5]    -- générique aux monoides
-- [1,2,3,4,3,4,5]

-- sur les listes la concaténation est en O(n)

plength :: List a -> PNat
plength Nil = Z
plength (Cons _ xs) = S (plength xs)

-- >>> plength (Cons "un" (Cons "deux" (Cons "trois" Nil)))
-- S (S (S Z))

longueur :: [a] -> Integer
longueur [] = 0
longueur (_:xs) = 1 + (longueur xs)

-- >>> longueur ["un", "deux", "trois"]
-- 3

-- Exercices : tout ce que vous avez fait sur les listes en ocaml, mais en Haskell

-- Question : quel est le principe d'induction sur les listes ?

{-

data List a =
  Nil
  | Cons a (List a)

-}

--   forall prop :: List a -> Bool
--     prop Nil == True
--     /\ (forall x :: a, forall xs :: List a,  prop xs == True ==> prop (Cons x xs) == True)
--     ==> forall xs : List a, prop xs == True

-- Exercice (pour aller plus loin):

-- Considérons une fonction map sur les listes :

myMap :: (a -> b) -> List a -> List b
myMap f Nil = Nil
myMap f (Cons x xs) = Cons (f x) (myMap f xs)

-- >>> myMap (\x -> x + 1) (Cons 0 (Cons 1 (Cons 2 Nil)))
-- Cons 1 (Cons 2 (Cons 3 Nil))

-- >>> fmap (\x -> x + 1)  [0, 1, 2]
-- [1,2,3]

-- >>> fmap (+1) [0, 1, 2]
-- [1,2,3]

-- On définit également un opérateur de compisition de fonction

comp :: (b -> c) -> (a -> b) -> a -> c
comp g f = \x -> g (f x)

-- >>> length ["a", "b", "c", "d"]
-- 4

-- >>> (\x -> x + 1) 4
-- 5

-- >>> (comp (\x -> x + 1) length) ["a", "b", "c", "d"]
-- 5

-- Remarque :  comp existe en Haskell, c'est l'opérateur infixe (.)

-- >>> ((\x -> x + 1) . length)  ["a", "b", "c", "d"]
-- 5

-- Prouver :

prop_mapcomp :: Eq c => (b -> c) -> (a -> b) -> List a -> Bool
prop_mapcomp g f xs =
  myMap (comp g f) xs == myMap g (myMap f xs)

-- Remarque : on doit pouvoir vérifier l'égalité sur le type c
--            (le type de retour de g et donc de myMap)

-- Petite question à propos de Num en fin de cours, voici quelques éléments.

-- >>> :t 42
-- 42 :: Num p => p   -- ceci veut dire : pour tout type p "numérique"

-- >>> :info Num-- type Num :: * -> Constraint
-- class Num a where
--   (+) :: a -> a -> a
--   (-) :: a -> a -> a
--   (*) :: a -> a -> a
--   negate :: a -> a
--   abs :: a -> a
--   signum :: a -> a
--   fromInteger :: Integer -> a
--   (etc...)

-- Cela veut dire qu'il faut implémenter les opérateurs +, -, *, etc.

-- >>> 42 :: Integer
-- 42

-- >>> 42 :: Int
-- 42

--  Integer (entiers arbitraires) et Int (entiers machine) implémentent
--  tous deux la typeclass Num,  autrement dit ils sont numérique.

-- Ce n'est pas le cas des booléens par exemple

-- >>> 42 :: Bool
-- <interactive>:4269:2-3: error:
--     • No instance for (Num Bool) arising from the literal ‘42’
--     • In the expression: 42 :: Bool
--       In an equation for ‘it’: it = 42 :: Bool


