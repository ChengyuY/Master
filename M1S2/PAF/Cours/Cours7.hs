{-# LANGUAGE InstanceSigs #-}

module PAFPAFPAF7Applicatives where


{-

Cours 7 : les foncteurs applicatifs
===================================

Plan du cours :
-------------

1. retour (rapide) sur les semigroupes et monoïdes

Exemples de Maybe et List

2. retour (rapide aussi, mais moins) sur les foncteurs

3. les foncteurs applicatifs

-}

-- On prend les "paraphrases" de deux (familles) de types standards :

data List a =
  Nil
  | Cons a (List a)
  deriving (Show, Eq, Ord)

data MyMaybe a =
  NNothing
  | JJust a
  deriving (Show, Eq, Ord)



{-

1. Rappel sur les semigroupes et monoïdes
-----------------------------------------

Pour un type T donné :

 - Semigroup  répond à la question :

> Comment concaténer/fusionner (avec <>) des valeurs de type T entre elles ?

(avec loi d'associativité)

 - Monoid est un semigroupe qui répond en plus à la question :

> Existe-t-il un élément neutre (nommé mempty) pour la concaténation/fusion ?

Regardons sur `List a` et `MyMaybe a`   (pour tout type `a`) :

-}

instance Semigroup (List a) where
  (<>) :: List a -> List a -> List a
  xs <> Nil = xs
  Nil <> xs = xs
  (Cons x xs) <> ys = Cons x (xs <> ys)

instance Monoid a => Monoid (List a) where
  mempty :: List a
  mempty = Nil

-- >>> Cons 1 (Cons 2 (Cons 3 Nil)) <> (Cons 4 (Cons 5 Nil))
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))


instance Semigroup a => Semigroup (MyMaybe a) where
  (<>) :: MyMaybe a -> MyMaybe a -> MyMaybe a
  x <> NNothing = x
  NNothing <> x = x
  JJust v <> JJust w = JJust $ v <> w

instance Monoid a => Monoid (MyMaybe a) where
  mempty :: MyMaybe a
  mempty = NNothing

-- Remarque : il n'y a pas de semigroupe/monoïde par défaut pour les entiers...

-- >>> (JJust (Sum 42)) <> NNothing
-- JJust (Sum {getSum = 42})

-- >>> (JJust (Sum 42)) <> (JJust (Sum 33))
-- JJust (Sum {getSum = 75})

-- >>> (JJust (Product 42)) <> (JJust (Product 33))
-- JJust (Product {getProduct = 1386})

-- >>> NNothing <> NNothing
-- NNothing


{-

2. Rappel sur les foncteurs
---------------------------

Pour les foncteurs, le point de départ est une structure/contexte manipulant, d'une
façon ou d'une autre, des valeurs d'un certain type, sans contrainte particulière
(souvent on donne à ce type polymorphe le nom a)

Donc on dispose d'un constructeur de type C paramétré par un type polymorphe, disons a.
Plus formellement, on a le kind  C :: * -> *

Exemples de constructeurs de types correspondant :

- les listes  []  et les listes "maison"  List

-- >>> :k []
-- [] :: * -> *

-- >>> :k List
-- List :: * -> *

- les valeurs optionnelles  Maybe ou MyMaybe  ou  (Either e)  pour tout type e

-- >>> :k Maybe
-- Maybe :: * -> *

-- >>> :k MyMaybe
-- MyMaybe :: * -> *

-- >>> :k (Either String)
-- (Either String) :: * -> *

-- >>> :k (Either Integer)
-- (Either Integer) :: * -> *

- les fonctions  ((->) e)  pour tout type e,
(peut-être un peu étonnament, on va en reparler : cela s'appelle le *reader*)

-- >>> :k (->)
-- (->) :: * -> * -> *

-- >>> :k ((->) Integer)
-- ((->) Integer) :: * -> *

-- >>> :k ((->) Bool)
-- ((->) Bool) :: * -> *

-- >>> :k ((->) Integer Bool)
-- ((->) Integer Bool) :: *

-- >>> :k (Integer -> Bool)
-- (Integer -> Bool) :: *

- les paires pour le second élement ((,) e) pour tout type e

-- >>> :k (,)
-- (,) :: * -> * -> *

-- >>> :k ((,) Integer)
-- ((,) Integer) :: * -> *

... Bref, tout contexte f qui peut avoir le kind  f :: * -> *
(cas général : un type somme-de-produits polymorphe)

Le foncteur répond alors à la question :

-  comment appliquer une fonction de type (a -> b)   sur les valeurs manipulées
dans le contexte/la structure f... Dit autrement, comment "entrer" dans la structure/le contexte pour en
modifier les valeurs ?

On parle de contexte fonctoriel, qui englobe une information factuelle - le kind * -> *
mais aussi une information plus "intentionnelle"

-}

{-

**Question** : Quelle interprétation "informelle" pour le contexte  Maybe/MyMaybe ?

Il s'agit du contexte des calculs optionnels.  On utilise Maybe parce que l'on n'est
pas sûr d'avoir une valeur au bout du calcul...

-}

instance Functor MyMaybe where
  fmap :: (a -> b) -> MyMaybe a -> MyMaybe b
  fmap g NNothing = NNothing
  fmap g (JJust v) = JJust (g v)

-- >>> fmap (+1) (JJust 42)
-- JJust 43

-- >>> fmap even (JJust 42)
-- JJust True

-- >>> fmap even (JJust 33)
-- JJust False

-- >>> fmap even NNothing
-- NNothing

{-
**Question** : Quelle interprétation pour le contexte  []/List ?

Deux intentions différentes possibles :

1) les listes comme conteneurs de valeurs (listes chaînées) ... c'est aussi
le contexte des séquences.   (=> c'est plutôt un contexte fini)

2) les calculs non-déterministes :  on a plusieurs valeurs possibles,  rangées
 dans un certain ordre, dans une liste  (potentiellement infinie).  Et que l'on considisère
 une-par-une ...  (contexte privilégié en Haskell)

-}

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap g Nil = Nil
  fmap g (Cons x xs) = Cons (g x) $ fmap g xs

-- >>> fmap (+1) (Cons 1 (Cons 2 (Cons 3 Nil)))
-- Cons 2 (Cons 3 (Cons 4 Nil))

-- >>> fmap even (Cons 1 (Cons 2 (Cons 3 Nil)))
-- Cons False (Cons True (Cons False Nil))

-- >>> fmap (+1) [1, 2, 3, 4, 5]
-- [2,3,4,5,6]

-- >>> take 5 $ fmap (+1) [1..]
-- [2,3,4,5,6]

{-

Propriété fondamentale : il n'existe qu'une seul foncteur possible pour un
contexte donné  (en garantissant les lois des foncteurs ...)

(dit autrement, lorsque l'on réalise une instance de Functor, il n'y a pas vraiment
de choix d'implémentation possible ... et du coup la dérivation automatique est 
possible ... et supportée avec l'extension DeriveFunctor ...)

-}

{-

3.  Les foncteurs applicatifs
-----------------------------

On se place dans la cadre d'un contexte fonctoriel f.
(et nos exemples du cours seront f=MyMaybe et f=List)

Motivation : appliquer des fonctions de plus d'un argument dans
mon contexte / ma structure.

Hors contexte on peut utiliser librement les fonctions
de "deux" arguments ... Par exemple, l'addition

-- >>> 2 + 40
-- 42

Ou, de façon équivalente :

-- >>> (+) 2 40
-- 42

Pour une fonction h :: a -> b -> c  et une valeur x :: a  et une valeur
 y :: b on peut librement invoquer   (h x y)  pour obtenir un c.


Si on a contexte fonctoriel f :: * -> *  avec une instance  (Functor f),
on peut appliquer librement une fonction  g :: a -> b  sur une structure
 s :: f a   (une valeur x :: a  dans le contexte de f)  avec  fmap g s

Par exemple : la fonction even :: Integer -> Bool  et (JJust 42) :: MyMaybe Integer
(donc g = even,  s = (JJuset 42),  f = MyMaybe et a = Integer)   peut être
librement appliquée avec  :

-- >>> fmap even (JJust 42)
-- JJust True

Problème : cela ne marche pas avec (strictement) plus d'un argument
Contre-exemple

-- >>> (+) (JJust 2) (JJust 40)
-- <interactive>:2416:2-25: error:
--     • Non type-variable argument in the constraint: Num (MyMaybe a)
--       (Use FlexibleContexts to permit this)
--     • When checking the inferred type
--         it :: forall a. (Num a, Num (MyMaybe a)) => MyMaybe a

Cela ne peut pas fonctionner car la fonction h :: a -> b -> c  est hors-contexte
 (ici  (+) :: Integer -> Integer -> Integer)   alors que les arguments sont
dans un contexte ... certes fonctoriel, mais cela ne marche quand même pas.

Pour résoudre ce problème, on introduit les foncteurs applicatifs.

La typeclass qui correspond se nomme  Applicative

-- >>> :info Applicative
-- type Applicative :: (* -> *) -> Constraint
-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

-- Remarque :   l'opérateur <*> se nomme "apply"

-- Cela ressemble beaucoup à Functor :

-- >>> :info Functor
-- type Functor :: (* -> *) -> Constraint
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

Dans Functor, le fmap <$> prend une fonction hors-context et l'applique sur
une valeur (l'argument) dans le contexte.

Alors que dans Applicative, le "apply" <*> prend une fonction dans le même
contexte que l'argument.


Revenons à un calcul qu'on aimerait par exemple effectuer :
(+) (JJust 2) (JJust 40)
Autrement dit : l'addition de deux valeurs optionnelles...

On peut déjà utiliser Functor pour prendre le premier argument :


-- >>> :t JJust (2 :: Integer)
-- JJust (2 :: Integer) :: MyMaybe Integer

Rappel :  fmap sur MyMaybe : fmap :: (a -> b) -> MyMaybe a -> MyMaybe b

-- On support  (+) :: Integer -> (Integer -> Integer)

-- on devrait avoir  a = Integer   et le b = (Integer -> Integer)
-- JJust 4  est bien du type  MyMaybe a  =  MyMaybe Integer

-- on devrait avoir en retour  MyMaybe b   donc  MyMaybe (Integer -> Integer)

-- >>> :t fmap (+) (JJust (2 :: Integer))
-- fmap (+) (JJust (2 :: Integer)) :: MyMaybe (Integer -> Integer)


Malheuresement, on ne sait pas (encore) quoi faire avec ce résultat... il nous
faut une instance d'Applicative ...

-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

On va définir des fonctions avec la bonne signature pour f = MyMaybe

-}

pureMyMaybe :: a -> MyMaybe a
pureMyMaybe = JJust

-- >>> pureMyMaybe (42 :: Integer) 
-- JJust 42

-- >>> pureMyMaybe True
-- JJust True

-- Donc "pure" c'est la réponse à : Comment entrer dans le contexte (fonctiorel)
-- pour une valeur hors-contexte...

applyMyMaybe :: MyMaybe (a -> b) -> MyMaybe a -> MyMaybe b
applyMyMaybe (JJust g) (JJust y) = JJust (g y)
applyMyMaybe _  _ = NNothing

instance Applicative MyMaybe where
  pure = pureMyMaybe
  (<*>) = applyMyMaybe

{-

On peut (enfin!) résoudre notre problème.

(+) (JJust 2) (JJust 40)

et on avait déjà la "moitié" de la solution :

fmap (+) (JJust 2)   :: MyMaybe (Integer -> Integer)

et JJust 40 :: MyMaybe Integer

... on peut conclure maintenant !

-- >>> applyMyMaybe (fmap (+) (JJust 2))  (JJust 40)
-- JJust 42

Mais, de façon plus lisible et plus générique :

-- >>> (fmap (+) (JJust 2)) <*>  (JJust 40)
-- JJust 42

Et si on se rappelle que l'opérateur <$> est un synonyme, on peut
réécrire cela de façon plus idiomatique :

-- >>> (+) <$> (JJust 2) <*> (JJust 40)
-- JJust 42

Et bien sûr, on a un principe général maintenant

-- >>> (&&) <$> (JJust True) <*> (JJust False)
-- JJust False

Dans le cas général, pour appliquer une fonction h :: a -> b -> c
dans un contexte f  on écrira   h <$> x <*> y     (avec x :: a et y :: b)  pour obtenir
un f c

Et cela fonctionne avec plus d'arguments, par exemple

 - pour hh :: a -> b -> c -> d   on écrira  hh <$> x1 <*> x2 <*> x3   pour obtenir un f d

...

 - pour hhh :: a1 -> a2 ... -> aN+1  on écrira   hhh <$> x1 <*> x2 <*> ... <*> xN
  pour obtenir un f aN+1

Remarque :  les instances sont déjà définies pour le "vrai" Maybe :

-- >>> (+) <$> (Just 4) <*> (Just 12)
-- Just 16

-- >>> (+) <$> (Just 4) <*> Nothing
-- Nothing

-}




{-

Deuxième exemple : les listes   (avec les listes maisons)

On prend l'interprétation  "calculs non-déterministes"   plutôt que l'interprétation
"conteneurs de valeurs"... donc plutôt "stream" ou "flot"  que  "liste stricte/tableau"

Rappel :

-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

On a déjà Functor :

-- >>> fmap even (Cons 1 (Cons 2 (Cons 3 Nil)))  
-- Cons False (Cons True (Cons False Nil))

-- >>> fmap even [1, 2, 3]
-- [False,True,False]

-}

pureList :: a -> List a
pureList x = Cons x Nil   -- dans les vrais listes : [x]

applyList1 :: List (a -> b) -> List a -> List b
applyList1 Nil _ = Nil
applyList1 (Cons g gs) Nil = Nil
applyList1 (Cons g gs) (Cons x xs) = Cons (g x) $ applyList1 gs xs

-- >>> applyList1 (Cons (+1) (Cons (\x -> x - 1) (Cons (*2) Nil))) (Cons 42 (Cons 42 (Cons 42 Nil)))
-- Cons 43 (Cons 41 (Cons 84 Nil))

-- Cela correspond à l'interprétation :  conteneur de valeurs   (et au contexte "ZipList", cf. TD)

-- Dans l'interprétation des calculs non-déterministes, on voudrait plutôt appliquer
-- chaque fonction g, ... etc (le reste gs)  à toutes les valeurs x, xs ...

applyList :: List (a -> b) -> List a -> List b
applyList Nil _ = Nil
applyList (Cons g gs) xs = (fmap g xs) <> (applyList gs xs)
-- fmap g xs :: List b
-- applyList gs xs :: List b

-- Heureusement : les List  sont bien un semigroup (et même un monoïde)

instance Applicative List where
  pure = pureList
  (<*>) = applyList

{-

Regardons ce que cela donne en pratique avec cette interprétation.

Application d'une fonction hors-contexte à un argument :

-- >>> (+1) <$> (Cons 1 (Cons 2 (Cons 3 Nil))) 
-- Cons 2 (Cons 3 (Cons 4 Nil))

-- >>> (*2) <$> (Cons 1 (Cons 2 (Cons 3 Nil)))
-- Cons 2 (Cons 4 (Cons 6 Nil))

-- >>> even <$> (Cons 1 (Cons 2 (Cons 3 Nil)))
-- Cons False (Cons True (Cons False Nil))

-- >>> (+1) <$> [1, 2, 3]
-- [2,3,4]

-- >>> (*2) <$> [1, 2, 3]
-- [2,4,6]

-- >>> even <$> [1, 2, 3]
-- [False,True,False]


Avec apply :

-- >>> (Cons (+1) (Cons (\x -> x - 1) (Cons (*2) Nil))) <*>  (Cons 4 (Cons 8 (Cons 12 Nil)))
-- Cons 5 (Cons 9 (Cons 13 (Cons 3 (Cons 7 (Cons 11 (Cons 8 (Cons 16 (Cons 24 Nil))))))))

-- >>> [(+1), (\x -> x - 1), (*2)] <*> [4, 8, 12]
-- [5,9,13,3,7,11,8,16,24]

-}

{-

Application d'une fonction hors-contexte à plus d'un argument :

-- >>> (+) <$> [1, 2, 3, 4] <*> [10, 20, 30]
-- [11,21,31,12,22,32,13,23,33,14,24,34]

-- c'est la même chose que

-- >>> [i + j | i <- [1, 2, 3, 4], j <- [10, 20, 30]]
-- [11,21,31,12,22,32,13,23,33,14,24,34]


-- Donc les listes "applicatives" sont des compréhensions ... suite au prochain cours...

-}
