
module Cours4 where

-- On va faire quelques folds ...
import Data.Foldable


{-

Cours 4 : Evaluation non-stricte, listes (co-)inductives et corécursion
=======================================================================

Plan du cours
-------------

1. Stratégies d'évaluation non-strictes

2. Listes (co-)inductives et corécursion

3. Quelques "subtilités" de l'évaluation non-stricte (ex. foldl vs foldl')

-}


{-

1. Stratégies d'évaluation non-strictes
---------------------------------------

Les expressions dans les langages de programmation :

 - des expressions formelles
 - d'un type donné  (pour les langages typés statiquement)
 - qui s'évaluent en ... une valeur   (~ forme normale)

En Haskell (comme dans la plupart des langages dits "fonctionnels" Ocaml, Scheme, Scala),
quasiment "tout" est une expression

Dans un langage impératif (C, Java ..) on sépare la notion d'instruction (principe d'exécution)
de la notion d'expression (principe d'évaluation)

-}

-- exemple
square :: Int -> Int
square z = z * z

dist :: Int -> Int -> Int
dist x y = square x + square y

-- Regardons l'évaluation de :

-- >>> dist (2 * 2) (1 + 1)
-- 20


-- Si on regarde dans les détails, pour passer de l'expression à une valeur
-- une certaine *stratégie d'évaluation* a été appliquée.

-- Dans la plupart des langages de programmation, la stratégie "par défault"
-- est l'évaluation stricte  (~ call-by-value ...)

{-
Exemple d'évaluation stricte

dist (2 * 2) (1 + 1)
 =>  dist 4 2             -- 1) évaluation des arguments
 =>  square 4 + square 2  -- 2) appel de la fonction (substitution des arguments)
 =>  4 * 4 + 2 * 2
 =>  20

Il existe d'autres stratégies possibles, par exemple :

dist (2 * 2) (1 + 1)
 => square (2 * 2) + square (1 + 1)       -- 1) appel d'abord
 => (2 * 2) * (2 * 2) + (1 + 1) * (1 + 1) -- 2) appel d'abord
 => 4 * 4 + 2 * 2
 => 20

Il s'agit d'une stratégie non-stricte (on n'évalue pas à l'avance les arguments)
Cette stratégie s'appelle (parfois) la stratégie "normale" et pour les appels
de fonctions cela s'appelle : le call-by-name

dist (2 * 2) (1 + 1)
 => square x + square y    -- x~~>(2 * 2)  et y~~(1 + 1)
 => z1 * z1 + z2 * z2      -- z1~~>x   et z2~~>y
 => (2 * 2) * (2 * 2) + (1 + 1) * (1 + 1)
 => 4 * 4 + 2 * 2
 => 20

Remarque : les deux stratégies conduisent à la même valeur/forme normale.
Cependant, les détails du calcul sont différents, en particulier le nombre d'étapes
(et aussi, l'utilisation de la mémoire)

La stratégie non-stricte/normale est ici moins efficace : en nombre de pas (CPU) et en mémoire

En stratégie stricte : on évalue les arguments (expressions) exactement une fois
En stratégie non-stricte/normale : il est possible d'évaluer plusieurs fois le même
argument (expression)

-}

-- deuxième exemple : l'implication

(==>) :: Bool -> Bool -> Bool
True ==> False = False
_ ==> _ = True

-- >>> (2 == 2) ==> (4 < 4)
-- False

-- >>> (2 < 2) ==> (4 < 4)
-- True

-- >>> (2 < 2) ==> ( (2 / 0) == 0)
-- True

{-
En stratégie stricte,   dans  e1 ==> e2  on aurait :
1) évaluer e1 et e2
2) puis appliquer la fonction "implication"

-}

-- Dans un langage "stricte" on aurait eu une exception ...

-- >>> (2 / 0)
-- Infinity

{-
En stratégie non-stricte/normale (à cause de pattern matching on n'est plus
tout à fait "normal", il faut au moins évaluer un argument) :

(2 < 2) ==> ( (2 / 0) == 0)
=> False ==> ( (2 / 0) == 0)
=> True

-- on n'a pas eu besoin d'évaluer e2

On voit donc un intérêt, entre autre, à l'évaluation non-stricte :
 implémenter des opérateurs logiques comme l'implication.

-}

-- on a vu aussi : le if-then-else fonctionnel, le and et le or

{-

Problème : la stratégie normale est inefficace car on peut évaluer plusieurs
fois la même expression...

Question : existe-t-il une stratégie non-stricte efficace ?

En théorie : oui, et cela s'appelle  l'évaluation paresseuse (lazy)

   ==> chaque expression est évaluée **au plus** une fois.

En pratique : la stratégie non-stricte est contrôlée par le langage, entre
 du lazy "pur" et du strict.  Donc en Haskell, on contrôle la "strictness", le
fait d'être strict ou non.

Avantages :
  - le compilateur fait une grosse partie du travail
  - on augmente l'expressivité (on peut exprimer plus de choses utiles, comme l'implication)
  - en théorie c'est plus efficace  (évaluation au plus une fois, donc si on n'a pas
    besoin d'une expression, elle ne sera potentiellement pas évaluée)

Inconvenient :
- en pratique, ça ajoute un peu de difficulté ... (cf. fin du cours)

-}

{-
Quelques aspects sur la stratégie non-stricte de Haskell/GHC

Premier aspect : stratégie par nom + pointeur

Exemple :

square :: Int -> Int
square z = z * z

dist :: Int -> Int -> Int
dist x y = square x + square y

dict (2 * 2) (1 + 1)    -- x~~>(2*2){4}   y~~>(1+1){2}
=> square x + square y  -- z1~~>x    z2~~>y
=> (z1 * z1) + (z2 * z2)
=> (4 * 4) + (z2 * z2)
=> (4 * 4) + (2 * 2)
=> 16 + 4
=> 20

-- Important : on a évalué z1 une fois, et z2 une fois

-}

{-

Second aspect : évaluation dirigée par le pattern matching

-}

sumFirst :: [Int] -> [Int] -> Int
sumFirst [] xs = 0
sumFirst (x:xs) [] = 0
sumFirst (x:xs) (y:ys) = x + y

-- >>> sumFirst [] [1+1, 2+2, 3+3]
-- 0

{-
sumFirst [] [1+1, 2+2, 3+3]   -- équation sumFirst.1
=> 0

-- Remarque : on n'a pas évalué le second argument, parce que le pattern
-- ne requiert pas d'évaluation
-}

-- >>> sumFirst [1+1, 2+2, 3+3] [] 
-- 0

{-
sumFirst [1+1, 2+2, 3+3] [] -- équation sumFirst.2
 == sumFirst (1+1:(2+2:3+3:[]))  -- x~~>(1+1), xs~~>[2+2, 3+3]
=> 0
-}

-- >>> sumFirst [1+1, 2+2, 3+3] [1*1, 2*2, 3*3]
-- 3

{-
sumFirst [1+1, 2+2, 3+3] [1*1, 2*2, 3*3]  -- équation sumFirst.3
                                          -- x~~>1+1{2}, xs~~>[2+2,3+3]
                                          -- y~~>1*1{1}, ys~~>[2*2,3*3]
=> x + y
=> 2 + 1
=> 3
-}



{-

2) Intérêt de disposer d'une stratégie non-stricte par défaut

a. la "composabilité"

b. les structures "infinies"

-}


{-

a. la "composabilité"
---------------------

Pour composer des fonctions "librement" et "efficacement", la stratégie non-stricte est importante.

Exemple : (sur les listes)

-}

-- myCompo :: [Integer] -> Integer
-- myCompo l = foldl' (+) 0 (fmap (\x -> x * x) (filter odd l))

-- en stratégie stricte :  on doit faire 3 parcours de la liste l
-- d'une certaine façon, on n'a pas vraiment "composer" les fonctions

-- en stratégie non-stricte : on n'a besoin de faire qu'une seul passage
-- (en lazy :  au plus un passage)

-- pour amplifier cet aspect "non-stricte = composable", je réécris la fonction :

myCompo :: [Integer] -> Integer
myCompo =  foldl' (+) 0 . fmap (\x -> x * x) . filter odd
-- on va beaucoup mieux ici le fait qu'on a composé des fonctions

-- >>> myCompo [1,2,3,4,5]
-- 35

-- cela conduit à ce que l'on appelle le style "point-free"

-- Exercice (un peu fastidieux) :  évaluer myCompo avec les explications de
-- la stratégie non-stricte de Haskell ...

-- Un deuxième exemple un peu plus "utile"

orList :: [Bool] -> Bool
orList [] = False
orList (b:bs) = b || orList bs

myAny :: (a -> Bool) -> [a] -> Bool  -- dans le prélude, cela s'appelle any
myAny p = orList . fmap p

-- >>> myAny (\x -> x == 0) [1, 9, 2, 4, 0, 4, 5, 3, 0, 3]
-- True

-- la stratégie non-stricte a évalué les éléments de la liste jusqu'au premier 0 et pas la suite

-- Exercice (un peu moins fastidieux) : faire l'évaluation en pas à pas

-- >>> :t (.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c

comp :: (b -> c) -> (a -> b) -> a -> c
comp g f x = g (f x)

myAny2 :: (a -> Bool) -> [a] -> Bool  -- dans le prélude, cela s'appelle any
myAny2 p = orList `comp` fmap p

-- en style "Ocaml" :
myAny3 :: (a -> Bool) -> [a] -> Bool  -- dans le prélude, cela s'appelle any
myAny3 p l = orList (fmap p l)


-- >>> myAny2 (\x -> x == 0) [1, 9, 2, 4, 0, 4, 5, 3, 0, 3]
-- True


{-

b. les structures "infinies" et la (co-)récursion

Le fait d'avoir une stratégie non-stricte par défaut augmente l'expressivité,
 notamment les listes deviennent "co-inductives"  donc plus proches des "streams"
que des listes "classiques".

Coinduction/corecursion : manipuler des structures et des calculs "infinis"
en faisant attention de n'observer que des parties finies des structures...

-}

-- une liste "infinie" de 1
ones :: [Integer]
ones = 1 : ones

-- ones   <=== boucle infinie !  on tente d'observer l'objet infini en entier

-- On peut observer une partie finie  (plus précisément un préfixe fini pour les listes)

-- >>> head ones      -- on aurait une boucle infinie en stricte
-- 1

-- >>> :t take
-- take :: Int -> [a] -> [a]

-- >>> take 10 ones
-- [1,1,1,1,1,1,1,1,1,1]

genNats :: Integer -> [Integer]
genNats n = n : genNats (n + 1)

-- >>> take 10 (genNats 3)
-- [3,4,5,6,7,8,9,10,11,12]

nats :: [Integer]
nats = genNats 1

-- >>> take 10 nats
-- [1,2,3,4,5,6,7,8,9,10]

-- il y a un raccourcis syntaxique en Haskell

nnats :: [Integer]
nnats = [1..]

-- >>> take 10 nnats
-- [1,2,3,4,5,6,7,8,9,10]

-- Expliquons take :

myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake 1 (x:_) = [x]
myTake n (x:xs) = x : myTake (n-1) xs

-- >>> myTake 10 nats
-- [1,2,3,4,5,6,7,8,9,10]

-- >>> myTake 10 [1..5]
-- [1,2,3,4,5]

-- Ici on a écrit une fonction récursive (sur n) qui observe une structure infinie


{-

Exemples de calculs sur les listes infinies

On va distinguer :

 - les générateurs qui décrivent les listes infinies/flots  (comme nats)

 - les transformateurs qui prennent une liste infinie/flots en entrée et construise une liste infinie/flot en sortie (comme fmap, filter)

 - les consommateurs qui observent un préfixe fini de la liste, et effectue potentiellement des calculus (take, foldl')  ==> les consommateurs produisent des valeurs (finies)

-}

-- Exemple de générateur "générique"

-- >>> :t iterate
-- iterate :: (a -> a) -> a -> [a]

-- construit la list [x, (f x), (f (f x)) ...]

-- >>> take 10 $ iterate (\x -> x + 1) 1
-- [1,2,3,4,5,6,7,8,9,10]

-- >>> take 10 $ iterate (\x -> x + x) 2
-- [2,4,8,16,32,64,128,256,512,1024]

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

-- >>> take 10 $ myIterate (\x -> x + x) 2
-- [2,4,8,16,32,64,128,256,512,1024]

-- Exemple de transformateur "générique"

-- >>> :t filter
-- filter :: (a -> Bool) -> [a] -> [a]


-- >>> take 10 $ filter even nats
-- [2,4,6,8,10,12,14,16,18,20]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter pred (x:xs)
  | pred x = x : myFilter pred xs
  | otherwise = myFilter pred xs

-- >>> take 10 $ myFilter even nats
-- [2,4,6,8,10,12,14,16,18,20]

-- Ces fonctions, myFilter et myIterate, quand on les considère sur
-- les listes infinies, ne sont pas récursives  (le case liste vide ne peut
-- pas se produire et donc il n'y pas de cas d'arrêt/de base)
-- ===> Ces fonctions sont dites co-récursives  
