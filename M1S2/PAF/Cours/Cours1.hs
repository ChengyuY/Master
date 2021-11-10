
module PAF_TranscriptCours1_2021 where

-- PAF Premier Cours : Modélisation avec les types
--                     (a.k.a. Type-Driven Development  TyDD ?)

-- Plan rapide du cours :
-- 1ère partie : Semaine 1 à 4 :  TyDD
-- Semaine 4 : Typeclasses   (classes de type)
-- 2ème partie : Structures algébriques
-- (foncteurs, applicatifs et ... les monades !)
-- 3ème partie : ouverture : (monad) transformers et les GADTs

-- Pour le premier cours : les types de base de Haskell, et la base commune
-- entre Ocaml et Haksell (types sommes, produits, records)

-- =====================================================================


-- 1) les booléens : le type Bool

-- le vrai s'appelle:  True
-- le faux s'appelle:  False

-- exemples de fonctions qui manipulent des booléens :

-- not dans le prelude
-- >>> not True
-- False

-- >>>  not False
-- True

-- >>> not (not True)
-- True

-- on peut réécrire la négation sous forme d'une fonction utilisateur

bnot :: Bool -> Bool   -- signature (type de la fonction)
bnot True = False      -- première équation  (gauche -> droite)
bnot False = True

-- && dans le prélude
band :: Bool -> Bool -> Bool
band True True = True
band _ _ = False

-- le type peut être inféré dans beaucoup de situations
-- >>> :t band
-- band :: Bool -> Bool -> Bool

-- >>> :t band True
-- band True :: Bool -> Bool

-- >>> :t band True False
-- band True False :: Bool

-- >>> band True (not False)
-- True

-- >>> band True (error "boum")
-- *** Exception: boum
-- CallStack (from HasCallStack):
--   error, called at <interactive>:454:13 in interactive:Ghci1

-- Remarque : en Haskell l'évaluation est "paresseuse" par défaut
-- >>> band False (error "boum")
-- False

-- >>> let f = band True in f True
-- True

-- style "point-free", on manipule les fonctions
-- >>> let f = band True in f False
-- False

-- style "pointed", on manipule les valeurs
-- >>> let f x = band True x in f False
-- False

-- >>> True `band` (not False)
-- True

-- exercice :  définir  le ou logique (||) bor
bor :: Bool -> Bool -> Bool
bor _ _ = undefined

-- >>> bor True True
-- *** Exception: Prelude.undefined
-- CallStack (from HasCallStack):
--   error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
--   undefined, called at /tmp/danteqtZt4I.hs:46:11 in main:Cours1

-- introduisons une notation infixe
(==>) :: Bool -> Bool -> Bool
(==>) True False = False
(==>) _ _ = True

infixr 1 ==>   -- opérateur infix associatif à droite (right)
               -- et de priorité 1    (sur 9)

-- >>> (2 > 3) ==> (2 > 4)
-- True

-- >>> (2 < 3) ==> (2 > 4)
-- False

-- >>> (==>) True False
-- False

-- le if,then,else peut s'écrire sous d'une fonction
bif :: Bool -> a -> a -> a
bif True ethen _ = ethen
bif False _ eelse = eelse

-- la grammaire propose un if-then-else
-- >>>  if  4 > 3  then  42 else 12
-- 42
-- >>>  if  4 < 3  then  42 else 12
-- 12

-- >>> bif (4 > 3) 42 12
-- 42

-- >>> bif (4 < 3) 42 12
-- 12

-- Exemple (intéressant) : l'utilisation des booléens pour définri
-- des propriétés.

prop_andCommutes :: Bool -> Bool -> Bool
prop_andCommutes a b = a && b ==> b && a

-- >>> prop_andCommutes True False
-- True

-- 2)  les entiers  soit Int  (entiers machine) soit Integer (grands entiers)

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

-- >>> fact 5
-- 120

factInt :: Int -> Int
factInt 0 = 1
factInt n = n * factInt (n - 1)

-- >>> factInt 5
-- 120

-- A propos de la récursion terminale ?
-- ==> en Haskell on est moins omnubilé par la récursion terminale
--     qu'en Scheme ou Ocaml par exemple.

-- Cependant, Haskell (ghc) élimine "la plupart" des appels terminaux

factit :: Integer -> Integer -> Integer
factit 0 acc = acc
factit n acc = factit (n - 1) (n * acc)

-- >>> factit 10 1
-- 3628800

-- En Haskell on va (très) rapidement privilégier des schémas de récursion :

-- >>> foldr (*) 1 [1..10]
-- 3628800

factit2 :: Integer -> Integer
factit2 n = foldr (*) 1 [1..n]

-- >>> factit2 10
-- 3628800

-- (un petit avant-goût de la suite)


factit3 :: Integer -> Integer
factit3 n = aux n 1
  where aux 0 acc = acc
        aux n acc = factit (n - 1) (n * acc)


-- =====================================================================


-- ===========  J'ai été trop bavard en cours et je n'ai pas
-- ===========  eu le temps de présenter la syntaxe des types
-- ===========  produits/sommes et records de Haskell en comparaison
-- ===========  des mêmes concepts en Ocaml...
-- ===========  Je fais un petit résumé ci-dessous mais ce sera de
-- ===========  tout façon rappelé en TD1 exercice 2.

-- =====================================================================


-- 3) Types sommes

{-

Les types somme "simples" correspondent, comme vous le savez, à
des unions disjointes.

Par exemple, en Ocaml, on pourrait définir un type booléen
de la façon suivante :

type mybool = True | False

En Haskell, la syntaxe est proche :

-}

data MyBool = BTrue | BFalse

{-

Il y a deux valeurs possibles du type `MyBool` :

 - `BTrue` pour vrai

 - `BFalse` pour faux

Ce sont les deux *constructeurs de données* pour le type  (data constructors)

-}

{-

On peut aussi bien sûr paramétrer par des types.
Voici un exemple :

-}

data MoyenPaiement =
  Cheque Int  -- numéro du chèque
  | Carte String String -- nom du porteur, numéro de CB
  | Espece

{-

Attention : en Haskell le nom du type et le nom des constructeurs
doivent commencer par une majuscule.

En Ocaml, on aurait pu écrire :

type moyenPaiement =
  Cheque of int
  | Carte of string * string
  | Espece

Et par exemple, on aurait pu écrire la fonction suivante :

let description mp = "Paiement par " ^
  match mp with
  | Cheque n -> "Cheque No" ^ (string_of_int n)
  | Carte (nom, num) -> "Carte No" ^ num ^ ", nom : " ^ nom
  | Espece -> "Espèce"


# description (Carte ("toto", "12345")) ;;

-: string = "Paiement par Carte No12345, nom : toto"


En Haskell on pourra traduire la fonction `description`
 de façon presque littérale :

-}

description :: MoyenPaiement -> String
description mp = "Paiement par " <>
  case mp of
    Cheque n -> "Cheque No" <> (show n)
    Carte nom num -> "Carte No" <> num <> ", nom : " <> nom
    Espece -> "Espèce"

-- >>> description (Carte "toto" "12345")
-- "Paiement par Carte No12345, nom : toto"

{-

Une version un peu plus élégante, disons "Haskelienne" :

-}

description2 :: MoyenPaiement -> String
description2 mp = "Paiement par " <> (describe mp)
  where describe (Cheque n) = "Cheque No" <> (show n)
        describe (Carte nom num) = "Carte No" <> num <> ", nom : " <> nom
        describe Espece = "Espèce"

-- on a transforme le `case ... of`  (le "match" de Haskell) en des équations,
-- plus dans l'esprit "équationnel" de Haskell.

-- =====================================================================

-- 4) Les types produits

{-

Haskell permet bien sûr de manipuler des n-uplets et autres
produits (carétésiens).

Par exemple :

>>> :t (2 :: Integer, "toto", True)
(2 :: Integer, "toto", True) :: (Integer, [Char], Bool)

Remarque : le "cast" explicite en Integer permet d'obtenir un type assez lisible ...
attention les chaînes de caractères par défaut sont des listes de caractères
avec le type `[Char]`   (les crochets se lisent "liste de").

Pour introduire un type produit spécifique, on peut utiliser exactement la
même syntaxe que pour les moyens de paiement, puisqu'il s'agissait bien sûr
d'un type "somme de produits".

Voici un autre exemple de "produit pur" :

-}

type Name = String
type Age = Int

data Person = Person Name Age
  deriving Show   -- on verra cette annotation en TME

{-

>>> :t Person "yoda" 700
Person "yoda" 700 :: Person

Ici les valeurs du type `Person` sont construite avec le constructur `Person`
(on utilise souvent cette convention de nommage pour les types unaires)
et qui attend deux paramètres : un nom et un age.

Remarque 1 :  `type` introduit un alias de type alors que `data` introduit un nouveau type.

Remarque 2 : `Person` est un type isomorphe mais non identique aux couples de type `(String, Int)` 

-}

-- Voici des fonctions "accesseurs" :

personName :: Person -> Name
personName (Person name _) = name

personAge :: Person -> Age
personAge (Person _ a) = a

{-

>>> personName (Person "yoda" 700)
"yoda"

>>> personAge (Person "yoda" 700)
700

-}


-- Et une petite opération :

anniversary :: Person -> Person
anniversary (Person name age) = Person name (age + 1)

-- >>> anniversary (Person "yoda" 700)
-- Person "yoda" 700

-- =====================================================================

-- 5) Les records

{-

Les types records (enregistrements) sont des sortes de "produits nommés".
Contrairement à Ocaml, les types records ne sont pas primitifs en Haskell,
 il s'agit simplement de sucre syntaxique pour des types produits "normaux".

Voici un exemple :

-}

data PersonInfo = PersonInfo {
  firstName, middleName, lastName :: Name
  , age :: Age
  , login :: String
  , birthYear :: Int }
  deriving Show

jean :: PersonInfo
jean = PersonInfo "jean" "jean" "jr" 42 "jean" 1956

-- >>> :t jean
-- jean :: PersonInfo

-- Expression de mise-à-jour (update)
setLogin :: PersonInfo -> String -> PersonInfo
setLogin pers logg = pers { login = logg }

-- >>> setLogin jean "michel"
-- PersonInfo {firstName = "jean", middleName = "jean", lastName = "jr", age = 42, login = "michel", birthYear = 1956}

-- Record utilisable comme un produit (en interne c'est un produit)
setLoginMoche :: PersonInfo -> String -> PersonInfo
setLoginMoche (PersonInfo f m l a _ b) logg = PersonInfo f m l a logg b

anniversaryBof :: PersonInfo -> PersonInfo
anniversaryBof (PersonInfo f m l a lg b) =
  PersonInfo f m l (a + 1) lg b

-- Avec l'expression d'update, c'est un peu mieux
anniversaryOk :: PersonInfo -> PersonInfo
anniversaryOk pers@(PersonInfo _ _ _ a _ _) = pers { age = a + 1 }

-- >>> anniversaryOk jean
-- PersonInfo {firstName = "jean", middleName = "jean", lastName = "jr", age = 43, login = "jean", birthYear = 1956}

-- Pattern matching de record
nameAge :: PersonInfo -> (Name, Age)
nameAge PersonInfo { lastName = n, age = a} = (n, a)

-- >>> nameAge jean
-- ("jr",42)

-- La version la plus concise
anniversaryBest :: PersonInfo -> PersonInfo
anniversaryBest pers@(PersonInfo { age = a}) = pers { age = a + 1}

-- >>> anniversaryBest jean
-- PersonInfo {firstName = "jean", middleName = "jean", lastName = "jr", age = 43, login = "jean", birthYear = 1956}

