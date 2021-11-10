
module PAF_TME8_GenMonad where

{-{

# TME 8 : Générateur aléatoire monadique de QuickCheck

Dans ce TME nous étudions la définition de générateurs aléatoires pour
le test basé sur les propriétés et QuickCheck. Ce thème est en rapport
avec le cours puisque l'architecture des générateurs de QuickCheck est
basée sur le concept de monade.

**Attention** : nous ne faisons quasiment pas de *property-based testing* dans
 ce TME, nous nous intéressons principalement à la définition de générateurs
 *custom* (le TME4 explique la partie test proprement dite).

Une fois n'est pas coutûme ce TME est plutôt du type tutoriel.
Le principe est de répondre aux différents **questions** et également,
d'évaluer dans GHCI (une fois le module chargé) tous les exemples
d'interactions  (vous pouvez remplacer les `???????` par les exemples
de valeurs obtenues). Comme nous travaillons sur des générateurs aléatoires,
 on obtient des valeurs potentiellement différentes à chaque exécution,
 donc il est utile d'exécuter plusieurs fois les mêmes interactions.

Pour ce TME, nous allons utiliser directement les définitions de
la bibliothèque QuickCheck.


}-}

import Test.QuickCheck
import System.Random (Random)
import Control.Applicative (liftA2)

{-{

# Première Partie : Architecture des générateurs QuickCheck

L'architecture des générateurs de QuickCheck est basée le type `Gen a` défini
de la façon suivante :

```haskell
>>> :info Gen
unknown command 'info'

L'information la plus importante est que `Gen` est un contexte monadique (et donc applicatif et
fonctoriel). L'intuition principale est que les générateurs se combinent essentiellement
avec *bind*, et donc que la notation `do` est disponible. Nous allons y revenir mais nous
allons d'abord exploiter la disponibilité d'un certain nombre de générateurs par défaut.

## Générateurs par défaut avec Arbitrary

Une autre composante importante de la génération aléatoire avec QuickCheck est
la *typeclasse* `Arbitrary`.

```haskell
>>> :info Arbitrary
unknown command 'info'

La fonction générique `arbitrary` peut être vue comme le générateur
par défaut pour le type `a`.   La fonction `shrink` implémente
(lorsque cela a du sens) un algorithme de réduction de donnée qui propose
une liste de valeurs «plus petites» à partir d'une valeur initiale.
Nous n'aborderons pas cette aspect dans le TME, mais c'est ce qui permet
à QuickCheck de trouver des contre-exemples les plus petits possibles
en cas de propriété invalidée.

Il existe de nombreuses instances de `arbitrary` prédéfinies, comme
par exemple des générateurs (et *shrinkers*) pour les types de base :

```haskell
instance Arbitrary Bool
instance Arbitrary Char
instance Arbitrary Integer
instance Arbitrary Int
instance Arbitrary Float
instance Arbitrary Double
```

Pour tester ces générateurs, nous pouvons utiliser les fonctions
`sample` ou `sample'` dont les signatures sont les suivantes :

```haskell
sample :: Show a => Gen a -> IO ()
sample' :: Gen a -> IO [a]
```

La première affiche la série de valeurs choisies arbitrairement,
 et la seconde retourne une liste. Toutes les deux ne peuvent
être utilisées que dans le contexte de `IO` car le générateur
aléatoire (de nombre) par défaut n'est pas pur (il utilise
une graine qui change à chaque initialisation du générateur).

Dans GHCI on peut par exemple écrire les expressions suivantes (après avec chargé le
 module `PAF_TME8_GenMonad`, ou directement avec `stack ghci`)  :

```haskell
>>> sample' (arbitrary :: Gen Integer)
[0,-2,4,-4,2,-5,8,4,0,11,-9]

>>> sample' (arbitrary :: Gen Integer)
[0,2,-3,0,-7,-3,12,9,4,-13,-19]

>>> sample' (arbitrary :: Gen Char)
"\SUB\15501S4\ACK@_\DC3l)'"

-- >>> sample' (arbitrary :: Gen Char)
-- "Z\1082697+\304938C\609601\557853\&9\246852\US\SUB"
```

Bien sûr, les résultats changent à chaque appel, ce sont des actions et
non des fonctions pures.

Il y a aussi des générateurs pour les types polymorphes de base :

```haskell
instance Arbitrary a => Arbitrary (Maybe a)
instance Arbitrary a => Arbitrary [a]
instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b)
instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (a, b, c)
-- etc ... jusqu'aux 10-uplets !
```

La fonction `sample'` retourne des listes de 11 données, ce qui est parfois
une peu trop gros, donc nous allons plutôt utiliser la fonction suivante :

}-}

samples :: Int -> Gen a -> IO [a]
samples n gen = do
  l <- sample' gen
  return $ take n l

{-{

Par exemple :

```haskel
>>> samples 4 (arbitrary :: Gen [Integer])
[[],[],[-1,-2,4],[-5,5,0,1,4]]

>>> samples 10 (arbitrary :: Gen (Maybe Int))
[Just 0,Nothing,Just 3,Nothing,Just 3,Just 3,Just 7,Nothing,Just 1,Just 10]

>>> samples 3 (arbitrary :: Gen (Double, Bool))
[(0.0,False),(1.4903746750359512,False),(1.2102443552836921,False)]

}-}

{-{

**Question :** : donner une expression avec `samples` permettant de générer des données
de la forme suivante :

```haskell
[[],[Left (-1),Left 2],[Right Nothing,Left 1,Right (Just False),Left 0]]
```

}-}


-- REPONDRE ICI

reponse :: IO [[Either Int (Maybe Bool)]]
reponse = samples 4 (arbitrary :: Gen [Either Int (Maybe Bool)])

{-{

# Générateurs custom

Nous allons maintenant nous intéresser à la définition proprement dite
de générateurs.

Pour cela, nous allons étudier un certain nombre de *combinateurs monadiques* de
génération. On a tout d'abord quelques générateurs que l'on pourrait appeler atomiques.

## Générateurs atomiques.

Le principe de génération élémentaire est un générateur uniforme d'entiers, que
l'on peut simplement utiliser pour générer des valeurs des types numériques de bases,
 ainsi que des types énumérés (comme `Bool` ou `Char`).
Ces générateurs atomiques sont fournis par le module `System.Random`
 de la bibliothèque *random* (cf. <https://hackage.haskell.org/package/random-1.1/docs/System-Random.html>). 

```haskell
>>> :info Random
class Random a where
  -- ... def pas importante ici --
  System.Random.randomR :: System.Random.RandomGen g =>
  	-- Defined in ‘System.Random’
instance Random Word -- Defined in ‘System.Random’
instance Random Integer -- Defined in ‘System.Random’
instance Random Int -- Defined in ‘System.Random’
instance Random Float -- Defined in ‘System.Random’
instance Random Double -- Defined in ‘System.Random’
instance Random Char -- Defined in ‘System.Random’
instance Random Bool -- Defined in ‘System.Random’
```

La fonction de quickcheck permettant de transformer une instance de
`Random` en un générateur est la suivante :

```haskell
choose :: Random a => (a, a) -> Gen a
```

Le premier argument est un couple `(m, n)` correspondant à un intervalle avec
`m` et `n` inclus:

```haskell
>>> sample' $ choose (4, 10)
???????

>>> sample' $ choose ('a', 'z')
???????
```

}-}


{-{

**Question** : Définir un générateur avec la signature suivante :

```haskell
chooseNat :: (Random a, Num a) => a -> Gen a
```
tel que `genNat n` ne génère que des nombres entre 1 et `n`

}-}

chooseNat :: (Random a, Num a) => a -> Gen a
chooseNat n = do 
  choose (1,n)

{-{

```haskell
>>> sample' $ chooseNat 10
Ambiguous type variable ‘a0’ arising from a use of ‘chooseNat’
prevents the constraint ‘(Random a0)’ from being solved.
Relevant bindings include
  it :: IO [a0]
    (bound at /Users/yangchengyu/Downloads/PAF/TME/paf-tme8-monades-master/src/PAF_TME8_GenMonad.hs:238:2)
Probable fix: use a type annotation to specify what ‘a0’ should be.
These potential instances exist:
  instance Random CChar -- Defined in ‘System.Random’
  instance Random CDouble -- Defined in ‘System.Random’
  instance Random CFloat -- Defined in ‘System.Random’
  ...plus 33 others
  (use -fprint-potential-instances to see them all)

}-}

{-{

## Combinateurs élémentaires

En complément des générateurs atomiques on trouve des opérateurs assez simples permettant de composer des générateurs atomiques.

Par exemple, la fonction `elements :: [a] -> Gen a` retourne un générateur constant
pour une valeur prise au hasard dans la liste spécifiée.

```haskell
>>> sample' $ elements [42, 2, 17]
[2,42,42,2,42,17,17,42,17,42,42]

>>> sample' $ elements ['a' .. 'e']
"bdbdcbcadce"

}-}

{-{

Le générateur `listOf :: Gen a -> Gen [a]` génère une liste de `a`.

```haskell
>>> samples 5 $ listOf (chooseNat 10) 
???????
```

La taille de la liste générée est bornée par un paramètre `size` qui
 est passé (via *bind* nous allons y revenir) entre générateurs.
Il est possible d'influencer sur la taille avec :

```haskell
resize :: Int -> Gen a -> Gen a
```

Par exemple :

```haskell
>>> samples 3 $ resize 10 $ listOf $ chooseNat 10
???????
```

Les deux premières listes sont de petite taille, mais la 3ème
est bien de longueur 10. En fait, l'indicateur de taille est
plus une sorte de demande informelle qu'un information à prendre
précisément en compte. Pour de nombreux générateurs prédéfinis,
 cette taille est prise comme une borne supérieure non-stricte,
 et les premières valeurs générées sont souvent de petite taille.
On verra un peu plus loin comment contrôler la taille un peu plus
finement.

Un autre générateur utile est `oneof:: [Gen a] -> Gen a`.
Il s'agit de choisir, de façon arbitrarire, un générateur parmi
la liste proposée. La contrainte est que tous les générateurs
sont pour le même type `a`.

Par exemple :

```haskell
>>> sample' $ oneof [choose (1, 10), choose (100, 110), choose (1000, 1010)]
???????
```

Un opérateur plus général permet de donner plus ou moins de poids à un
générateur. Le poids est indiqué par un entier, et bien sûr une valeur
plus grande augment la probabilité d'utiliser le générateur correspondant.

La signature de cette fonction est la suivante :

```haskell
frequency :: [(Int, Gen a)] -> Gen a
```

**Attention**: il ne faut pas voir ici un contrôle fin de la distribution
aléatoire, car cette dernière n'est pas maîtrisable du fait de l'enchaînement
de générateurs arbitraires. On fait ici plus de la génération *arbitraire* que
de la *véritable* génération aléatoire (avec contrôle de la distribution des
valeurs générées).

Voici une variante de l'exemple précédent :

```haskell
>>> sample' $ frequency [(60, choose (1, 10)), (30, choose (100, 110)), (10, choose (1000, 1010))]
???????
```

On aurait ici, *grosso modo*, 60% de chance de tirer un nombre entre 1 et 19, 30% entre 100 et 110 et 10% entre 1000 et 1010.

Voici une petite vérification «maison» de cette distribution attendue :

}-}

genFreq :: Gen Integer
genFreq = frequency [(60, choose (1, 10)), (30, choose (100, 110)), (10, choose (1000, 1010))]

freqStats :: (Num a, Ord a) => [a] -> (Double, Double, Double)
freqStats xs =
  aux 0 0 0 xs
  where aux nb1 nb2 nb3 [] =
          let tot = nb1 + nb2 + nb3
              coef = 100.0 / (fromIntegral tot)
          in (fromIntegral nb1 * coef, fromIntegral nb2 * coef, fromIntegral nb3 * coef)
        aux nb1 nb2 nb3 (x:xs) | x <= 10 = aux (nb1+1) nb2 nb3 xs
                               | x <= 110 = aux nb1 (nb2+1) nb3 xs
                               | otherwise = aux nb1 nb2 (nb3+1) xs

checkFrequency :: (Random a, Num a, Ord a) => Gen [a] -> IO (Double, Double, Double) 
checkFrequency gen = do
  xs <- generate gen
  return $ freqStats xs

{-{

Essayez d'invoquer plusieurs fois cette fonction de vérification de la fréquence.

```haskell
>>> checkFrequency $ resize 100000 $ listOf genFreq
???????
```

On obtient des résultats plutôt cohérents avec la spécification, mais tout cela dépend des générateurs utilisés.

}-}

{-{

Le dernier générateur que l'on présente permet de filtrer des valeurs en entrées, par un principe
de *génération avec rejet*. L'idée est très simple : on prend des valeurs en entrées et on ne conserve
que celle qui vérifie une propriété. Le combinateur concerné possède la signature suivante :

```haskell
suchThat :: Gen a -> (a -> Bool) -> Gen a
```

Si l'on veut par exemple uniquement générer des entiers pairs, on pourra utiliser l'expression
suivante :

```haskell
>>> sample' $ chooseNat 100 `suchThat` even
???????
```

**Attention** : il ne faut pas abuser du `suchThat`   (que l'on utilise le plus souvent en
position infixe, pour des questions de lisibilité) car les valeurs rejetées peuvent être nombreuses.
En particulier, il faut être à peu près certain que le prédicat concerné est «souvent» vrai pour
le générateur qu'il filtre. Voici un bon contre-exemple :

```haskell
>>> sample' $ chooseNat 100000 `suchThat` (==1)
???????
```

Ici, *QuickCheck* a bien retourné une liste de 1 mais après un temps assez long,
 et l'exemple suivant ne s'arrête bien sûr jamais (ne pas le lancer !) :

```haskell
>>> sample' $ choose (1, 10) `suchThat` (==11)
-- ... infinite loop
```

Il existe d'autres combinateurs intéressants dans la bibliothèque fournie par *QuickCheck*,
 on n'hésitera pas à consulter la documentation associée (rubrique *Generator combinators*).

}-}


{-{

## Combinateurs monadiques

On peut créer des générateurs intéressants avec les générateurs atomiques et
les combinateurs élémentaires. Cependant, pour avoir un contrôle plus fin de la
génération, il faut exploiter le fait que les fonctions de signature
`a -> Gen a` sont monadiques, c'est-à-dire peuvent être combinées pour produire
des générateurs complexe.

Nous allons exploiter le fait que `Gen` est un contexte monadique. Notamment,
 si on a un générateur `gen` de type `Gen a` et une fonction monadique `f` de type
`a -> Gen b` alors l'expression `gen >>= f` produit un générateur `Gen b` qui dépend
potentiellement de `gen`.

Le générateur le plus simple que l'on puisse imaginer est celui qui génère toujours
la même valeur. On cherche donc la signature `a -> Gen a` et c'est le fameux
`pure` des applicatifs  (aussi connu comme le `return` des monades).

Par exemple :

```haskell
>>> sample' $ (pure 42 :: Gen Int)
???????
```

Illustrons maintenant l'utilisation du bind. Considérons comme exemple un générateur qui inverse les valeurs produites
par un générateur de nombres.

}-}

chooseInv :: Num a => Gen a -> Gen a
chooseInv gen = gen >>= (\x -> return (- x)) 

{-{

Par exemple :

```haskell
>>> sample' $ chooseInv $ chooseNat 10
???????
```

**Question** : utiliser la notation `do` pour définir une fonction `chooseInv2` qui
définie exactement le même générateur.

}-}

chooseInv2 :: Num a => Gen a -> Gen a
chooseInv2 gen = 
  do
    x <- gen
    (\a -> return (- a)) x

{-{

Par exemple :

```haskell
>>> sample' $ chooseInv2 $ chooseNat 10
???????
```
}-}  

{-{

A présent, étudions la génération de couples de valeurs. On peut être étonné
de ne pas voir dans la bibliothèque de combinateur avec une signature comparable
à la suivante :

```haskell
genPair :: Gen a -> Gen b -> Gen (a, b)
```

Dans la bibliothèque tierce *checkers* il existe l'opérateur  `(>*<)` mais il est très facile de créer un tel générateur, ici en
exploitant la notation `do` :

}-}

genPair :: Gen a -> Gen b -> Gen (a, b)
genPair genFst genSnd = do
  x <- genFst
  y <- genSnd
  return (x,y)

{-{

Par exemple :

```haskell
>>> samples 5 $ genPair (chooseNat 10) (arbitrary :: Gen Bool)
???????
```
}-}

{-{

**Question** : version applicative

On peut voir que `x` et `y` sont traités de façon indépendantes, on peut même dire parallèle,
puisque on aurait pu les tirer dans un autre sens. Dans ce genre de situation, ou l'aspect
séquentiel des monades n'est pas exploité, le contexte applicatif est souvent suffisant.
A partir de cette remarque, définir un générateur `genPair2` qui génère
des couples dans un contexte applicatif.

}-}

-- genPair2 :: Applicative f => f a -> f b -> f (a, b)
genPair2 :: Gen a -> Gen b -> Gen (a, b)
genPair2 m n = (,) <$> m <*> n

{-{

Par exemple :

```haskell
>>> samples 5 $ genPair2 (chooseNat 10) (arbitrary :: Gen Bool)
???????
```

----

}-}

{-{

On peut bien sûr généraliser à tout type produit. Prenons l'exemple suivant :

}-}

data Personne = Personne { nom :: String, prenom :: String, age :: Int }
  deriving (Show, Eq)

{-{

**Question** : Définir un générateur `genPersonne` pour ce type. Pour le
nom est le prénom on indiquera une taille désirée de 10, et
pour l'age se sera entre 7 et 99 ans.
}-}

genPersonne :: Gen Personne
genPersonne = Personne <$> vectorOf 10 (choose ('a', 'z')) <*> vectorOf 10 (choose ('a', 'z')) <*> choose (7,99)

{-{

Par exemple :

```haskell
>>> samples 2 $ genPersonne
[Personne {nom = "nzqmnmorcy", prenom = "nswgfdrivp", age = 51},Personne {nom = "hrunteagze", prenom = "empbatplwf", age = 83}]

}-}

{-{

Bien sûr, on peut définir des générateurs récursifs pour les types somme (et somme de produits).

Voici par exemple un générateur alternatif pour `Maybe` :

}-}

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe gen = do
  x <- arbitrary -- ici on tire un booléen
  if x then gen >>= (pure . Just)
  else return Nothing  -- ou  do { x <- gen ; pure (Just x) }

{-{

**Remarque** : bien sûr on aurait pu utiliser `oneof` mais on voit ici une autre possibilité.

Par exemple :

```haskell
>>> samples 6 $ genMaybe $ choose (1, 10)
???????
```

}-}

{-{

**Question** : Soit le type suivant

}-}

data Geom = Point
  | Rect { longueur :: Int, largeur :: Int }
  | Circle { rayon :: Int }
  deriving (Show, Eq)

{-{

Définir un générateur `genGeom` qui tire dans 20% des cas un point,
 dans 50% des cas un rectangle de longueur dans l'intervalle (5, 60) et de largeur
 minimale 1 et maximale 40 (en faisant attention que la longueur soit au moins aussi grande que la largeur).
Dans les autres cas un cercle de rayon entre 1 et 10.

}-}

genGeom :: Gen Geom
genGeom = frequency [ (20, pure Point),
                      (50, uncurry Rect <$> (do
                                                l <- choose (5,60)
                                                w <- choose (1,40) `suchThat` (<= l)
                                                return (l,w)) ),
                      (30, Circle <$> choose (1,10))
                    ]


{-{

Par exemple :

```haskell
>>> samples 3 genGeom
[Circle {rayon = 3},Rect {longueur = 58, largeur = 25},Rect {longueur = 26, largeur = 25}]

}-}

{-{

## Générateurs récursifs

On peut bien sûr implémenter un algorithme récursif de génération.

En guise d'illustration, nous pouvons définir un générateur pour nombres naturels,
 en partant du type suivant :

}-}

data Nat = Z | S Nat
  deriving (Show, Eq, Ord)

{-{

Comme tout type récursif, le principe est de générer un arbre dont les feuilles
sont des constructeurs atomiques (comme `Z` pour zéro) et les noeuds internes sont
les constructeurs récursifs (ici `S` pour successeur).

}-}

genNat :: Gen Nat
genNat = do
  m <- getSize  -- on utilise le paramètre de taille comme borne max
  n <- choose (1, m)
  suivre n Z
    where suivre :: Int -> Nat -> Gen Nat
          suivre n k | n > 0 = suivre (n-1) (S k)
                     | otherwise = return k

{-{

Par exemple :

```haskell
>>> samples 1 $ resize 10 $ genNat
[S (S (S (S (S (S (S (S (S Z))))))))]

}-}


{-{

On se rappelle que le générateur `listOf` interprète le paramètre de taille
des générateurs comme un ordre de grandeur. On peut donc définir une
variante qui interprête la taille de manière plus stricte.

**Question** : définir un générateur `listOfSize :: Int -> Gen a -> Gen [a]`
qui retourne systématiquement des listes de la taille spécifiée en argument.

}-}

listOfSize :: Gen a -> Int -> Gen [a]
listOfSize l = suivre
  where suivre cpt 
          | cpt <= 0 = pure []
          | otherwise =  liftA2 (:) l (suivre (cpt - 1)) 


{-{

Par exemple :

```haskell
>>> samples 4 $ listOfSize (chooseNat 10) 5
???????
```

Nous pouvons en déduire un combinateur qui utilise le paramètre de taille
implicite de la monade `Gen`.

}-}

sizedList :: Gen a -> Gen [a]
sizedList gen = do
  size <- getSize
  listOfSize gen size

{-{

Par exemple :

```haskell
>>> samples 4 $ resize 5 $ sizedList $ chooseNat 10
???????
```

Il existe une autre façon d'écrire des variantes pour les générateurs de la
forme `Int -> Gen a` avec la fonction QuickCheck suivante :

```haskell
sized :: (Int -> Gen a) -> Gen a
```

Ici on peut donc écrire :

}-}

sizedList' :: Gen a -> Gen [a]
sizedList' gen = sized (listOfSize gen) 

{-{

Par exemple :

```haskell
>>> samples 4 $ resize 5 $ sizedList' $ chooseNat 10
???????
```

}-}

{-{

Nous allons prendre comme dernier exemple récursif le cas des arbres binaires.
Nous partons du type suivant :

}-}

data BinTree a = Tip | Node a (BinTree a) (BinTree a)
  deriving (Show, Eq)

{-{

Nous allons supposer que la taille de l'arbre correspond au nombres
de noeuds internes `Node`.

Un générateur naif est par exemple le suivant :

}-}

genBinTreeNaive :: Gen a -> Int -> Gen (BinTree a)
genBinTreeNaive _ 0 = return Tip
genBinTreeNaive gen 1 = gen >>= \x -> return $ Node x Tip Tip
genBinTreeNaive gen size = do
  x <- gen
  lsize <- choose (0, size-1)
  l <- genBinTreeNaive gen lsize
  r <- genBinTreeNaive gen (size - lsize)
  return $ Node x l r

{-{

Par exemple :

```haskell
>>> samples 1 $ genBinTreeNaive (choose (1, 5)) 5
??????
```

}-}

{-{

**Question** : Définir un générateur `GenBinTree` qui utilise le paramètre implicite de taillle
de la monade `Gen`.

}-}

genBinTree :: Gen a -> Gen (BinTree a)
genBinTree gen = do
  n <- getSize 
  genBinTreeNaive gen n

{-{

Par exemple :

```haskell
>>> samples 1 $ resize 5 $ genBinTree (choose (1, 5))
???????
```
----

}-}

