
{-# LANGUAGE OverloadedStrings #-}

module PAFCours3 where

-- on va utiliser le text
import Data.Text (Text)
import qualified Data.Text as Text

-- imports nécessires pour les séquences
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq   -- ou S ou le préfixe que vous voulez, Seq dans le cours PAF

-- utilitaires de tables associatives
import Data.Map (Map)
import qualified Data.Map as Map

-- utilitaires d'ensembles
import Data.Set (Set)
import qualified Data.Set as Set

{-

# Thème 1 - TyDD - Cours 3 : Architecture "PAF" + types conteneurs

Cours en deux parties :

1) petit récap. méthodologique et technique

2) les structures discrètes  : séquences, tables associatives (map) et ensembles (set)

-}


{-

## Petit récap. méthodologique

On commence donc par un récapitulatif sur la modélisation dirigée par les types
et l'architecture des projets PAF/Haskell

L 'architecture générale des projets développés en Haskell dans le contexte du cours
PAF est (légèrement) inspirée du principe du
Functional Domain Modelling et (plus fortement) de ce que l'on
appelle les méthodes formelles de G.L. (méthodes Z, B, VDM, etc.)

(cf. figure)

-}

{-

### Principe 1 : les entités (du domain) sont des sommes-de-produits (records)

Et on distingue les entités d'un côté et les valeurs de programmation sans domaine spécifique
(pour implémenter des algorithmes)...

-}


data Tank =
  Tank Integer Integer
  | FullTank Integer
  | EmptyTank Integer
  deriving (Show)

capacity :: Tank -> Integer
capacity (Tank _ cap) = cap
capacity (FullTank cap) = cap
capacity (EmptyTank cap) = cap

quantity :: Tank -> Integer
quantity (Tank qty _) = qty
quantity (FullTank cap) = cap
quantity (EmptyTank cap) = 0

{-

### Principe 2 : les états remarquables ont des constructeurs spécifiques
(voir des types spécifiques)

Exemple pour la cuve :  le fait d'être vide, pleine, ou remplie partiellement

-}

{-

### Principe 3 : les états incohérents ne sont pas constructibles

Exemple d'état incohérent : une cuve avec la quantité supérieure à la capacité

-}

mauvaiseCuve :: Tank
mauvaiseCuve = Tank 20 10   -- 20L dans une cuve de 10L ?????

-- la propriété suivante est un invariant sur les cuves
prop_cuveCorrect :: Tank -> Bool
prop_cuveCorrect (Tank qty cap) = 0 <= qty && qty < cap
prop_cuveCorrect (EmptyTank cap) = cap > 0
prop_cuveCorrect (FullTank cap) = cap > 0

prop_tank_inv = prop_cuveCorrect

-- >>> prop_cuveCorrect mauvaiseCuve
-- False

-- Suggestion : faire un constructeur qui vérifie ==> smart constructeur
mkTank :: Integer -> Maybe Tank    -- encore mieux : avec Either et des cas d'erreurs
mkTank cap | cap > 0 = Just $ EmptyTank cap
           | otherwise = Nothing

-- à prouver : forall cap :: Integer,  cap > 0 ==> prop_cuveCorrect (fromJust (mkTank cap))

{-

### Principe 4 : définition de propriétés sur les entités : les invariants
et sur les fonctions

Sur les entités, ce sont des invariants
Sur les fonctions, on va distinguer les préconditions et les postconditions

Précondition : propriété assumée vraie avant d'appeler la fonction
Postcondition : propriété espérée variant après l'appel de la fonction
                (exemple : la préservation de l'invariant)

-}

-- initialisation
initTank_pre :: Integer -> Bool
initTank_pre cap = cap > 0

initTank :: Integer -> Maybe Tank
initTank cap = undefined

prop_initTank_inv :: Integer -> Bool
prop_initTank_inv cap =
  initTank_pre cap
  && case initTank cap of
       Just tank -> prop_tank_inv tank
       Nothing -> False -- initialisation should work

-- transition
fillTank_pre :: Tank -> Integer -> Bool
fillTank_pre tank vol = ((quantity tank) + vol) <= (capacity tank)   

fillTank :: Tank -> Integer -> Maybe Tank
fillTank tank vol = undefined

prop_fillTank_inv :: Tank -> Integer -> Bool
prop_fillTank_inv beforeTank vol =
  prop_tank_inv beforeTank
  && fillTank_pre beforeTank vol
  && case fillTank beforeTank vol of
       Just afterTank -> prop_tank_inv afterTank
       Nothing -> False -- on n'a pas le droit à un non-événement... 

{-{

**Remarque** : la traduction des propriétés pour qu'elles soient exploitables
dans quickcheck n'est pas forcément trivial, mais on a déjà là quelque chose
d'intéressant à tester de façon classique (cf. Hspec).

}-}

-- exemple de postcondition supplémentaire (uniquement intéressante avant d'implémenter)

fillTank_positive_post :: Tank -> Integer -> Tank -> Bool
fillTank_positive_post beforeTank vol afterTank =
  prop_tank_inv beforeTank
  && fillTank_pre beforeTank vol
  && prop_tank_inv afterTank
  && quantity afterTank == quantity beforeTank + vol

{-

### Principe 5 : découvrir et formaliser (implémenter) des combinateurs avec
leurs propriétés "algébriques"

Inspiration : algebra-oriented programming (à chercher sur google)

Les entités se combinent entre elles : par exemple, connecter des cuves
avec des "tuyaux" et d'autres entités ...

On verra ça aux cours 6, 7, 8 avec les structures algébriques ...

-}


{-

# Partie 2 : les conteneurs (structures discrètes)

Fournies par la bibliothèque  containers

-}

{-

Les séquences : le type Seq a  des listes plus efficaces pour la plupart
des opérations que le type [a]   (avec des opérations efficaces en plus)

-}

-- exemple de construction avec Seq.fromList

-- >>> :t Seq.fromList [1, 2, 3, 4, 5 :: Integer]
-- Seq.fromList [1, 2, 3, 4, 5 :: Integer] :: Seq Integer

mySeq :: Seq Text
mySeq = Seq.fromList ["a", "b", "c", "d", "e"]

-- accès au premier et au dernier élément

seqFirst :: Seq a -> Maybe a
seqFirst (x :<| _) = Just x
seqFirst _ = Nothing

seqLast :: Seq a -> Maybe a
seqLast (_ :|> x) = Just x
seqLast _ = Nothing

-- >>> seqFirst mySeq
-- Just "a"

-- >>> seqLast mySeq
-- Just "e"

-- Accès en O(log(n)) aux indices   avec Seq.index

-- >>> Seq.index mySeq 0
-- "a"
-- >>> Seq.index mySeq 2
-- "c"

-- pour manipuler les séquences, on peut écrire des fonctions récursives
-- mais en "vraie" programmation fonctionnelle on utilise plutôt des combinateurs

-- en particulier : map (plutôt fmap), filter et foldr

carres :: Seq Integer -> Seq Integer
carres xs = fmap (\x -> x * x) xs

-- >>> carres (Seq.fromList [1, 2, 3, 4, 5])
-- fromList [1,4,9,16,25]

-- petit exercice de cours : faire un map sur les séquences avec une fonction récursive

-- foldrWithIndex :: (Int -> a -> b -> b) -> b -> Seq a -> b 

somme :: Seq Integer -> Integer
somme xs = Seq.foldrWithIndex (\ idx elem res -> res + elem) 0 xs

-- >>> somme (Seq.fromList [1, 2, 3, 4, 5])
-- 15

produit :: Seq Integer -> Integer
produit xs = Seq.foldrWithIndex (\ idx elem res -> res * elem) 1 xs

-- >>> produit (Seq.fromList [1, 2, 3, 4, 5])
-- 120

-- Un bon exo : une fonction qui retourne le maximum dans une séquence
-- accompagné de sont indice. ex  maxEtIndice [2, 5, 4, 3, 1]  --> (5, 1)
-- avec forldWithIndex  (et directement, récursivement)

{-

## Les tables associatives

Une table d'associations entre des clés uniquées et une valeur associée
Le type des clé doit être ordonné (typeclass Ord qu'on peut en général
dériver ...)

-}

-- exemple de construction : ici à partir du type  [(Text, Integer)]

myMap :: Map Text Integer
myMap = Map.fromList [("a", 1), ("b", 2), ("c", 3), ("d", 4)]

{-

Opération de recherche d'une valeur à partir de sa clé avec Map.lookup

-}

-- >>> Map.lookup (Text.pack "c") myMap
-- Just 3

-- >>> Map.lookup (Text.pack "e") myMap
-- Nothing


{-

Exemple d'opération : un fold sur les associations (clé, valeur)

-}

-- >>> :t Map.foldrWithKey
-- Map.foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b

addLast :: a -> Seq a -> Seq a
addLast x Empty = Empty :|> x    -- petit oublie : Empty est la séquence vide
addLast x xs = xs :|> x

-- addLast == :|>           <=== donc vous voyez, je dis pas toujours des trucs intelligents

-- >>> addLast (Text.pack "toto") mySeq
-- fromList ["a","b","c","d","e","toto"]

-- Exercice : addFirst


listKeys :: Map k a -> Seq k
listKeys m = Map.foldrWithKey (\cle val res -> res :|> cle) Empty m

-- >>> listKeys myMap
-- fromList ["d","c","b","a"]

-- Exercice : les listVals

{-

## Les ensembles

Structure linéaire mais sans ordre séquentiel et sans répétition

-}

-- construction

mySet :: Set Integer
mySet = Set.fromList [1, 2, 1, 3, 3, 5, 4, 5, 1]

-- >>> mySet
-- fromList [1,2,3,4,5]

-- "ajouter" un élément avec insert

-- >>> :t Set.insert
-- Set.insert :: Ord a => a -> Set a -> Set a

-- >>> Set.insert 12 mySet
-- fromList [1,2,3,4,5,12]

-- test d'appartenance

-- >>> Set.member 2 mySet
-- True

-- >>> Set.member 12 mySet
-- False

-- sous-ensemble

-- >>> Set.isSubsetOf mySet (Set.fromList [2, 3, 4])
-- False

-- >>> mySet `Set.union`  (Set.fromList [12, 3, 9])
-- fromList [1,2,3,4,5,9,12]






