
{-# LANGUAGE InstanceSigs #-}

module PAF8Algebras where

import Test.QuickCheck (quickCheck, Arbitrary, Gen, arbitrary, forAll, Property)
import qualified Test.QuickCheck as QC

{-

# Cours 6 : Les structures algébriques (1/4) : foncteurs, semigroups, monoïdes (et foldables)

**Remarque** : le principal support utilisé pour la rédaction de ce cours est la Typeclassopedia.
cf. https://wiki.haskell.org/Typeclassopedia

-}




{-


## Préambule : la notion de Catégorie

cf. document annexe

La catégorie qui nous intéresse s'appelle "Hask":

 - les objets sont des types (non-polymorphes) : Integer, Bool, [Int], Map String Bool ...

- les morphismes (flèches) sont les fonctions pures et totales entre les objets (types)
   even :: Integer -> Bool,   show :: Integer -> String      (donc il y a plusieurs even, plus show ...)


Question :  qu'est-ce-que l'identité ?

-- >>> :t id
-- id :: a -> a

-- ce sont toutes les identités :  Integer -> Integer,   Bool -> Bool, etc.   (pour tout type a)

-}

identity :: a -> a
identity x = x

-- les lois de l'identité :

-- en théorie : f . id  = f
law_Cat_rightId :: Eq b => (a -> b) -> a -> Bool
law_Cat_rightId f x = (f . identity) x == f x

-- en théorie : id . f = f
law_Cat_leftId :: Eq b => (a -> b) -> a -> Bool
law_Cat_leftId f x = (identity . f) x == f x

-- >>> law_Cat_leftId show (3 :: Integer)    -- show :: Integer -> String
-- True

-- >>> law_Cat_rightId (+1) (3 :: Integer)   --  (+1) :: Integer -> Integer
-- True

-- la loi d'associativité de la composition

law_Cat_assoc :: Eq d => (c -> d) -> (b -> c) -> (a -> b) -> a -> Bool
law_Cat_assoc h g f x = (h . (g . f)) x == ((h . g) . f) x

{-

Rappel :

-- >>> :t (.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- (g . f) x = g (f x)

-}

{-
Preuve (que la propriété est toujours vérifiée) :

(h . (g . f)) x
= h ((g . f) x)     {- définition de (.) gauche->droite -}
= h (g (f x))       {- définition de (.) g->d -}
= (h . g) (f x)     {- définition de (.) d->g -}
= ((h . g) . f) x   {- définition de (.) d->g -}

<CQFD>
-}


{-

2. La notion de foncteur
========================

On prend la catégorie Hask ... avec les objets sont des types monomorphes (simples) et
les flèches sont des fonctions pures et totales....  Alors, un exemple de foncteur (le plus simple possible)
est basé sur le type suivant :

-}

newtype Box a = Box a
  deriving (Show, Eq)

-- >>> :k Box
-- Box :: * -> *

{-

Pour faire de Box un foncteur, il faut qu'à partir de par exemple, une fonction f
de a -> b  (par exemple de Integer -> Bool)  on puisse construire un  Box b (Box Bool)
à partir d'un Box a (Box Integer).

-}

mapBox :: (a -> b) -> Box a -> Box b
mapBox g (Box x) = Box (g x)

{-

En Haskell, le principe général du "map" utilise la notion de foncteur (dans les catégories)
et introduit la typeclasse suivante :

-- >>> :info Functor
-- type Functor :: (* -> *) -> Constraint
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
--   (<$) :: a -> f b -> f a
--   {-# MINIMAL fmap #-}

L'important est de pouvoir implémenter fmap

Exemple : instanciation avec Box

-}

instance Functor Box where
  -- fmap :: (a -> b) -> Box a -> Box b
  fmap = mapBox

-- >>> fmap even (Box 42)
-- Box True

-- >>> fmap even (Box 33)
-- Box False

-- >>> fmap (+1) (Box 42)
-- Box 43

{-

Box est un foncteur minimal (et donc peu intéressant en pratique) mais il y a d'autres
foncteurs intéressants :  par exemple, les listes

-- >>> fmap (+1) [1, 2, 3, 4]
-- [2,3,4,5]

Ou alors Maybe :

-- >>> fmap (+1) (Just 42)
-- Just 43

-- >>> fmap (+1) Nothing
-- Nothing


(également les séquences ...)

-}


{-

Les lois des foncteurs en Haskell :

-}

law_Functor_id :: (Eq (f b), Functor f) => (a -> b) -> f a -> Bool
-- en théorie :  fmap identity = identity
-- en pratique :
law_Functor_id g x = (fmap g (identity x)) == (identity (fmap g x))

law_Functor_comp :: (Functor f, Eq (f c)) => (b -> c) -> (a -> b) -> f a -> Bool
-- en théorie : fmap (g . f) = fmap g . fmap f
-- en pratique :
law_Functor_comp g f x = fmap (g . f) x == (fmap g . fmap f) x

-- Exercice "maison" : on peut prouver ces lois sur Box...

{-

Un point important de terminologie :   le `f`  dans `Functor f`   se nomme
le **contexte fonctoriel**   (l'important est : contexte,  ou contexte de calcul)

Exemples de contextes fonctoriels :  `Box`,  `Maybe`,  `[]`

Rappel : il sont tous le kind  * -> *

-}

{-

Exercice de cours :  instancier Functor sur une version "maison" de Either

-}

data MyEither a b = MyLeft a | MyRight b
  deriving (Show, Eq)

-- Question : comment instancier Functor sur cette structure ?

-- Que choisir comme contexte fonctoriel ?  Ce contexte doit être de kind : * -> *

-- Problème :

-- >>> :k MyEither
-- MyEither :: * -> * -> *

-- Comment faire pour obtenir quelque chose de la forme * -> * ?

-- Comment "mapper" un MyEither ?

mapMyEither1 :: (a -> b) -> MyEither a c  -> MyEither b c
mapMyEither1 f (MyLeft x) = MyLeft (f x)
mapMyEither1 f (MyRight y) = MyRight y

-- >>> mapMyEither1 (+1) (MyLeft 42)
-- MyLeft 43

-- >>> mapMyEither1 (+1) (MyRight True)
-- MyRight True

mapMyEither2 :: (a -> b) -> MyEither c a  -> MyEither c b
mapMyEither2 f (MyRight x) = MyRight (f x)
mapMyEither2 f (MyLeft y) = MyLeft y

-- >>> mapMyEither2 (+1) (MyLeft True)
-- MyLeft True

-- >>> mapMyEither2 (+1) (MyRight 42)
-- MyRight 43

-- Maintenant qu'on a "réussi" à "mapper" des MyEithers ... essayons
-- de constuire un Functor   (instancier Functor)
-- à partir d'une de ces fonctions ...


-- >>> :info Functor-- type Functor :: (* -> *) -> Constraint
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
--   (<$) :: a -> f b -> f a
--   {-# MINIMAL fmap #-}

-- >>> :k MyEither
-- MyEither :: * -> * -> *

-- >>> :k MyEither Bool Integer
-- MyEither Bool Integer :: *

-- >>> :k MyEither Bool
-- MyEither Bool :: * -> *

-- >>> :k MyEither Integer
-- MyEither Integer :: * -> *

-- >>> :k MyEither [Integer]
-- MyEither [Integer] :: * -> *

instance Functor (MyEither c) where
  fmap :: (a -> b) -> MyEither c a -> MyEither c b
  fmap = mapMyEither2

-- >>> fmap (*2) (MyLeft True)
-- MyLeft True

-- = mapMyEither2 (*2) (MyLeft True)     {- définition du fmap pour (MyEither c) -}
-- = MyLeft True                         {- équation mapMyEither2.2 -}


-- >>> fmap (*2) (MyRight 42)
-- MyRight 84

{-

3. Les semigroupes et les monoïdes  (et l'application aux foldable)
-------------------------------------------------------------------

Ce sont des structures algébriques très utilisées en maths abstraites ... et en programmation ?

En Haskell, comme souvent, les structures vont être introduites par une typeclasse dédiée :

-- >>> :info Semigroup
-- type Semigroup :: * -> Constraint
-- class Semigroup a where
--   (<>) :: a -> a -> a

il s'agit de l'abstraction de la notion de "concaténation"

Exemple : sur des "listes maisons"

-}

data List a = Nil | Cons a (List a)
  deriving (Show, Eq)

-- >>> :t Cons 42 (Cons 33 (Cons 55 Nil))
-- Cons 42 (Cons 33 (Cons 55 Nil)) :: Num a => List a

-- >>> :t [42, 33, 55]
-- [42, 33, 55] :: Num a => [a]

{-

Pour Semigroup, il faut se poser la question : qu'est-ce qu'une "concaténation" pour a structure ?

-}

listConcat :: List a -> List a -> List a
listConcat Nil ys = ys
listConcat xs Nil = xs
listConcat (Cons x xs) ys = Cons x (listConcat xs ys)

-- >>> listConcat (Cons 42 (Cons 33 (Cons 55 Nil)))  (Cons 99 (Cons 101 Nil))
-- Cons 42 (Cons 33 (Cons 55 (Cons 99 (Cons 101 Nil))))

-- Remarque :
-- >>> :k List
-- List :: * -> *

instance Semigroup (List a) where
  (<>) :: List a -> List a -> List a
  (<>) = listConcat

-- >>> (Cons 42 (Cons 33 (Cons 55 Nil))) <> (Cons 99 (Cons 101 Nil))
-- Cons 42 (Cons 33 (Cons 55 (Cons 99 (Cons 101 Nil))))

-- Les listes "standard" sont aussi un semi-groupe :

-- >>> [42, 33, 55] <> [99, 101]
-- [42,33,55,99,101]

-- Les textes aussi ... les séquences ...

law_Semigroup_assoc :: (Eq a, Semigroup a) => a -> a -> a -> Bool
law_Semigroup_assoc x y z = x <> (y <> z) == (x <> y) <> z

{-

Un monoïde c'est un semi-groupe avec en plus une notion d'élément neutre.

En haskell, voici la typeclasse correspondante :

-- >>> :info Monoid
-- type Monoid :: * -> Constraint
-- class Semigroup a => Monoid a where
--   mempty :: a
--   mappend :: a -> a -> a
--   mconcat :: [a] -> a
--   {-# MINIMAL mempty #-}

Pour définir un monoïde il faut une notion d'élément neutre pour la concaténation (<>).

Sur notre type liste, on cherche xx tel que :  xs <> xx  ==  xx <> xs == xs

On prend   xx = Nil     (et dans le type liste standard c'est la liste vide  [])

-}

instance Monoid (List a) where
  mempty :: (List a)
  mempty = Nil

-- >>> mempty <> (Cons 42 (Cons 33 Nil))
-- Cons 42 (Cons 33 Nil)

-- >>> mempty <> [42, 33]
-- [42,33]

-- >>> (Cons 42 (Cons 33 Nil)) <> mempty
-- Cons 42 (Cons 33 Nil)

-- >>> [42, 33] <> mempty
-- [42,33]


-- >>> mconcat [[1, 2, 3, 4], [5, 6, 7], [8, 9, 10, 11]]
-- [1,2,3,4,5,6,7,8,9,10,11]

-- Idem sur nos listes "maison"


{-

En TD :
 - on va voir  un exemple de  Semigroup qui n'est pas un monoïde   (c'est rare ...)

 - on va utiliser le concept de monoïde dans le cadre de la typeclasse Foldable

-}
