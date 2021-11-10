
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cours5 where

import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Read as Read

import Data.Foldable (Foldable)
import qualified Data.Foldable as F


{-

Cours 5 : Typeclasses
=====================

Plan du cours :

 1) typeclasses, contraintes de types et polymorphisme ad-hoc

 2) typeclasses de base : Eq, Ord, Show ... et Data.Foldable

 3) définition de typeclasse (et comparaison avec les interfaces en OO)

-}

{-

 1) Typeclasses, contraintes de types et polymorphisme ad-hoc

Motivation (à l'origine) : polymorphisme ad-hoc

Normalement, vous connaissez le polymorphisme paramétrique

Exemple : la fonction identité

-}

identity :: a -> a
identity x = x

-- >>> identity 42
-- 42

-- >>> identity True
-- True


{-

Cette fonction est polymorphe, pour tout type a.
(ça correspond au polymorphisme de ML)


Avantage :  un même comportement pour n'importe quel type

Inconvénient : on n'a pas le choix, le comportement doit être indépendant du type


Exemple : calcul du périmètre

-}

periInt :: Integer -> Integer -> Integer
periInt x y = 2 * (x + y)

periFloat :: Float -> Float -> Float
periFloat x y = 2 * (x + y)

-- Point commun entre Integer et Float : les opérateurs arithmétiques

{-

Plusieurs manières d'aborder le problème :

1) ne pas vraiment le résoudre

Exemple en Ocaml

# let periInt x y = 2 * (x + y)
val periInt : int -> int -> int = <fun>

# let periFloat x y = 2 *. (x +. y);;
Line 1, characters 20-21:
1 | let periFloat x y = 2 *. (x +. y);;
                        ^
Error: This expression has type int but an expression was expected of type
         float
# let periFloat x y = 2.0 *. (x +. y);;
val periFloat : float -> float -> float = <fun>

Une bonne raison :   les algorithmes sous-jacents et leurs propriétés sont très différentes.
Exemple : l'addition entière est (en gros) associative    mais pas l'addition flottante

Au final c'est quand même un peu "lourd" à l'usage   (avis personnel)

-}


{-

2) des mécanismes ad-hoc  (conversions implicites et explicites)

Exemple : C, C++ et Java

Donc un entier est converti au besoin en flottant  (attention aux débordements)
Inconvénient : les bugs (potentiels) associés
Inconvénient (C, Java) : si on veut un nouveau type numérique, on ne peut pas utiliser
les opérateurs usuels (ce n'est pas extensible)...

-}

{-

3) Le polymorphisme ad-hoc avec les typeclasses et les contraintes de type
(Haskell, Scala, Purescript...)

-}

peri :: Num a => a -> a -> a
peri x y = 2 * (x + y)

-- >>> :t peri
-- peri :: Num a => a -> a -> a

-- >>> :t 2
-- 2 :: Num a => a

-- >>> :t (+)
-- (+) :: Num a => a -> a -> a

{-

On a ici des signatures avec une partie "type" :   a -> a -> a

et une partie "contraintes de type"  :   Num a => ...

Ici, la contrainte indique :  la signature est valable pour tout type a
qui vérifie la contrainte Num a.

Que signifie Num ?

-- >>> :info Num
-- type Num :: * -> Constraint
-- class Num a where
--   (+) :: a -> a -> a
--   (-) :: a -> a -> a
--   (*) :: a -> a -> a
--   negate :: a -> a
--   abs :: a -> a
--   signum :: a -> a
--   fromInteger :: Integer -> a
--   {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
--   	-- Defined in ‘GHC.Num’
-- instance Num Word -- Defined in ‘GHC.Num’
-- instance Num Integer -- Defined in ‘GHC.Num’
-- instance Num Int -- Defined in ‘GHC.Num’
-- instance Num Float -- Defined in ‘GHC.Float’
-- instance Num Double -- Defined in ‘GHC.Float’


Donc la fonction peri fonctionnera sur les entiers et les flottants
qui sont bien instances de cette typeclasse,  donc on a bien les
contraintes (Num Integer)  et (Num Float)

-}

-- >>> peri (2::Integer) 5
-- 14

-- >>> :t peri (2::Integer) 5
-- peri (2::Integer) 5 :: Integer

-- >>> peri 2.3 5
-- 14.6

-- >>> :t peri 2.3 5
-- peri 2.3 5 :: Fractional a => a

-- >>> :t peri (2.0 :: Double) 5
-- peri (2.0 :: Double) 5 :: Double

{-

2) Les typeclasses de base
--------------------------
(dans le prélude)

a.  La classe Eq   pour l'égalité
---------------------------------

En Java, on exploite le polymorphisme de sous-typage (avec .equals)
En Ocaml, on a une égalité structurelle commune   (compare)  ,  idem en Scheme, en Clojure ...

Haskell exploite (bien sûr) le polymorphisme ad-hoc avec la typeclass Eq

-- >>> :info Eq
-- type Eq :: * -> Constraint
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--   {-# MINIMAL (==) | (/=) #-}


Pour tester l'égalité entre deux expressions    expr1 == expr2    avec expr1, expr2 de type a
il faut que le type a vérifie la constrainte (Eq a)

Exemple sur les entiers :

-- >>> 42 == 12+30
-- True

-- >>> 42 /= 12+30
-- False

Pourquoi on n'a pas d'égalité structurelle unique :

1) dans certaines situations, on veut implémenter un algorithme différent  (optimisations)

2) certaines "choses" ne sont pas comparables, typiquement les fonctions

-- >>> (\x -> x) == (\y -> y)
-- <interactive>:1199:2-23: error:
--     • No instance for (Eq (p0 -> p0)) arising from a use of ‘==’
--         (maybe you haven't applied a function to enough arguments?)
--     • In the expression: (\ x -> x) == (\ y -> y)
--       In an equation for ‘it’: it = (\ x -> x) == (\ y -> y)


Exemple d'instanciation manuelle de la typeclasse Eq :

-}

data CarModel = Sedan | Coupe | SUV | Convertible
  deriving Show

-- >>> :t Coupe
-- Coupe :: CarModel

-- >>> Sedan == Coupe
-- <interactive>:1282:2-15: error:
--     • No instance for (Eq CarModel) arising from a use of ‘==’
--     • In the expression: Sedan == Coupe
--       In an equation for ‘it’: it = Sedan == Coupe

eqCar :: CarModel -> CarModel -> Bool
eqCar Sedan Sedan = True
eqCar Coupe Coupe = True
eqCar SUV SUV = True
eqCar Convertible Convertible = True
eqCar _ _ = False

-- >>> eqCar Sedan Coupe
-- False

-- >>> eqCar Sedan Sedan
-- True

-- On peut instancier la typeclasse Eq

instance Eq CarModel where
  (==) = eqCar

-- >>> Sedan == Coupe
-- False

-- >>> Sedan /= Coupe
-- True

-- >>> Sedan == Sedan
-- True

{-

En pratique, on va plutôt dériver Eq si on veut un test d'égalité structurelle.

-}

data CarModel' = Sedan' | Coupe' | SUV' | Convertible'
  deriving (Show, Eq)


-- >>> Sedan' == Coupe'
-- False

-- >>> Sedan' /= Coupe'
-- True

-- >>> Sedan' == Sedan'
-- True

-- La plupart des typeclasses "intéressantes" sont associées à des "lois" (propriétés)
-- qu'il faut respecter.

{-{

### Les lois de l'égalité

Les typeclasses s'accompagnent souvent de **lois** que les instances doivent respecter. Pour l'égalité, les règles usuelles sont les suivantes :

- *Reflexivité* : `x == x <=> True`

}-}

law_Eq_refl :: Eq a => a -> Bool
law_Eq_refl x = x == x

{-{

- *Symétrie* : `x == y <=> y == x`

}-}

law_Eq_sym :: Eq a => a -> a -> Bool
law_Eq_sym x y = (x == y) == (y == x)

{-{

- *Transitivité* : `if x == y && y == z = True, then x == z = True`

}-}

infixr 2 ==>
(==>) :: Bool -> Bool -> Bool
(==>) a b = (not a) || b

law_Eq_trans :: Eq a => a -> a -> a -> Bool
law_Eq_trans x y z = (x == y) && (y == z) ==> x == z

{-{

- *Substitutivité* : `if x == y = True, then f x == f y = True`

}-}

law_Eq_subst :: (Eq a, Eq b) => a -> a -> (a -> b) -> Bool
law_Eq_subst x y f = x == y ==> f x == f y

{-{


- *Négation* : `x /= y <=> not (x == y)`

}-}

law_Eq_neg :: Eq a => a -> a -> Bool
law_Eq_neg x y = (x /= y) == not (x == y)

{-{


Pour des raisons techniques, certaines instances ne respectent pas certains lois, notamment avec les flottants (type `Double`), mais dans la mesure du possible il est clairement préférable de les respecter.

}-}

{-

b.  La classe Ord   pour l'ordre "naturel"
------------------------------------------

-- >>> :info Ord
-- type Ord :: * -> Constraint
-- class Eq a => Ord a where
--   compare :: a -> a -> Ordering
--   (<) :: a -> a -> Bool
--   (<=) :: a -> a -> Bool
--   (>) :: a -> a -> Bool
--   (>=) :: a -> a -> Bool
--   max :: a -> a -> a
--   min :: a -> a -> a
--   {-# MINIMAL compare | (<=) #-}


-- >>> :info Ordering
-- type Ordering :: *
-- data Ordering = LT | EQ | GT

Exemple : ordonner les couples (paires)

-}

data Pair a b = Pair a b
  deriving (Show, Eq)

-- >>> :t Pair (2 :: Integer) True
-- Pair (2 :: Integer) True :: Pair Integer Bool


infPair :: (Ord a, Ord b) => Pair a b -> Pair a b -> Bool
infPair (Pair x1 y1) (Pair x2 y2) = (x1 < x2) || ((x1 == x2) && (y1 <= y2))

-- Important : il faut que x1 et x2 soient comparables, dont on a la contrainte (Ord a)
--             et pareil pour y1 et y2 donc avec la contrainte (Ord b)

-- >>> infPair (Pair 2 4) (Pair 3 2)
-- True

-- >>> infPair (Pair 4 4) (Pair 3 2)
-- False

-- >>> infPair (Pair 2 3) (Pair 1 5)
-- False

instance (Ord a, Ord b) => Ord (Pair a b) where
  (<=) = infPair

-- >>> (Pair 2 4) <= (Pair 3 2)
-- True

-- >>> (Pair 4 4) <= (Pair 3 2)
-- False

-- >>> (Pair 2 3) <= (Pair 1 5)
-- False

-- >>> (Pair 2 3) > (Pair 1 5)
-- True

-- >>> compare (Pair 2 3) (Pair 1 5)
-- GT


-- Remarque : on peut définir un autre ordre sur les paires, par exemple l'ordre "parallèle"
-- Problème : on peut pas instancier deux fois la même typeclasse pour un type donné.

-- Donc, il faut introduire un nouveau type.

data Pair' a b = Pair' a b
  deriving (Show, Eq)

instance (Ord a, Ord b) => Ord (Pair' a b) where
  (Pair' x1 y1) <= (Pair' x2 y2) = (x1 <= x2) && (y1 <= y2)

-- >>> (Pair' 2 4) <= (Pair' 3 2)
-- False

-- >>> (Pair' 4 4) <= (Pair' 3 2)
-- False

-- >>> (Pair' 2 3) <= (Pair' 1 5)
-- False

-- En pratique, on va plutôt utiliser l'ordre structurel (pour les paires)


data Pair'' a b = Pair'' a b
  deriving (Show, Eq, Ord)

-- >>> (Pair'' 2 4) <= (Pair'' 3 2)
-- True

-- >>> (Pair'' 4 4) <= (Pair'' 3 2)
-- False

-- >>> (Pair'' 2 3) <= (Pair'' 1 5)
-- False

{-

Autres typeclasses : Show, Foldable, ...

-}

data Tree a = Tip | Node a (Tree a) (Tree a)
  deriving (Show, Eq, Ord, Foldable)

exTree :: Tree Integer
exTree = Node 42 (Node 3 Tip Tip) (Node 5 Tip Tip)

-- >>> F.foldl' (+) 0 exTree
-- 50

-- >>> F.toList exTree
-- [42,3,5]

-- >>> F.all (>0) exTree
-- True

{-

Les classes numériques (cf. TME),   les classes algébriques (cf. cours 6, 7 et 8)
(etc.)

-}

{-

3. Définition des typeclasses (comparaison avec les interfaces OO / Java)
------------------------------

Warning : il est assez rare d'implémenter soi-même une nouvelle typeclasse,  il y a notamment
des "anti-patterns"  (cf. articles blogs) ... c'est une première différence avec les interfaces
(notamment en Java ou en C#).

==> dans ce cours, je "casse" un peu cette bonne pratique

Il y a quand même un point commun :  la définition de signatures (de fonctions, ou méthodes) à
implémenter.

On veut une interface/typeclasse pour la représentation textuelle  (pour le type Text).

En Java, on pourrait écrire :

public interface Printable {
  public String toStr();
}

-}

class Printable a where
  toStr :: a -> Text

-- ici, on dit qu'un type a est "Printable" si il implémente toStr avec
-- la bonne signature

{-

En Java, il faut une classe (donc un type *et* une implémentation) pour définir un type "Printable"

public class MyInt implements Printable {
  public final int value;
  public MyInt(int value) { this. value = value; }

  public String toStr() {
    return Integer.toString(value);
  }
}

En Haskell, on pourrait "simuler" un peu la même chose de la façon suivante :

-}

newtype MyInt = MyInt Integer
  deriving (Show, Eq, Ord)

instance Printable MyInt where
  toStr :: MyInt -> Text
  toStr (MyInt n) = Text.pack $ show n

-- >>> toStr (MyInt 42)
-- "42"

-- On pourrait encapsuler un booléen, un flottant ... et les rendre "Printable", comme en Java

{-

Mais en Haskell, les typeclasses sont ce que l'on appelle un système (d'extension) ouvert.
On peut notamment rendre "Printable"  un type déjà défini   (et qui n'est pas déjà Printable).

Plus généralement, on peut instancier une typeclasse sur un type existant
(ce qui n'est pas possible en OO).

-}

instance Printable Integer where
  toStr :: Integer -> Text
  toStr n = Text.pack $ show n

-- >>> toStr 42
-- "42"

instance Printable Bool where
  toStr :: Bool -> Text
  toStr n = Text.pack $ show n


-- >>> toStr True
-- "True"

instance Printable a => Printable (Maybe a) where
  -- toStr :: Printable a => Maybe a -> Text
  toStr Nothing = "Nothing"
  toStr (Just v) = "Just " <> (toStr v)

-- >>> toStr (Just 42)
-- "Just 42"

-- >>> toStr (Nothing  :: Maybe Integer)
-- "Nothing"

-- >>> toStr Nothing   -- sans annotation supplémentaire ne marche pas, ce qui montre le côté un peu
--                     -- non-trivial de la sélection de la bonne implémentation...
--                     -- car Nothing est défini pour tout type a et pas juste ceux Printable ...


-- Remarque : comme suggéré en cours, on peut faire une version
-- générique à partir de Show... Mais cela demande des extensions de langages
-- (suggérées par le typeur)

instance Show a => Printable a where
  toStr n = Text.pack $ show n

-- >>> toStr 4.2
-- "4.2"

-- >>> toStr "hello"
-- "\"hello\""

-- Problème pour `Bool` par exemple, il y a plusieurs instances possibles !
-- >>> toStr True
-- <interactive>:3756:2-11: error:
--     • Overlapping instances for Printable Bool
--         arising from a use of ‘toStr’
--       Matching instances:
--         instance [safe] Show a => Printable a
--           -- Defined at /tmp/dantehOPePP.hs:596:10
--         instance [safe] Printable Bool
--           -- Defined at /tmp/dantehOPePP.hs:568:10
--     • In the expression: toStr True
--       In an equation for ‘it’: it = toStr True

-- pour résoudre le problème, on a deux solutions:
-- 1) enlever l'instance la moins générique, mais si on voulait un affichage différent c'est raté
-- 2) au contraire enlever l'instance générique et accepter de dupliquer un peu de code....

-- Important : les classes sont résolues dans les modules, donc deux modules différents peuvent
-- utiliser des instances différentes.


{-

Le polymorphisme du type de retour.

Les contraintes de types s'appliquent aussi bien sur les types des arguments d'une fonction
que sur le type de retour.

Exemple : le complémentaire de Printable
-------
-}

class Readable a where
  fromStr :: Text -> Maybe a

-- un premier cas facile
instance Readable Text where
  fromStr :: Text -> Maybe Text
  fromStr = Just   -- ou  fromStr s = Just s

-- IMPORTANT :
-- Quand on veut exploiter le polymorphisme du type de retour (et non des arguments)
-- il faut "expliquer ce que l'on veut"  (sinon le typage est ambigû).

-- >>> (fromStr "hello") :: Maybe Text
-- Just "hello"

-- un cas un peu plus difficile
instance Readable Integer where
  fromStr :: Text -> Maybe Integer
  fromStr s = case Read.decimal s  of  -- parsing d'un décimal dans Text.Read
                Left _ -> Nothing
                Right (n :: Integer, _) -> Just n
-- Remarque : l'annotation de type pour `n` est nécessaire.

-- >>> (fromStr "42")  :: Maybe Integer
-- Just 42

{-

Une autre différence importante avec les interfaces est le dispatch :

- en OO, le dispatch est dynamique pour l'argument "this" (premier argument implicite)
  et en Java, il y a aussi la surcharge (dispatch statique)

- en Haskell, le dispatch est statique et sur tous les arguments et le type de retour
  (ce qui oblige a parfois indiquer "ce que l'on veut" ...)

-}
