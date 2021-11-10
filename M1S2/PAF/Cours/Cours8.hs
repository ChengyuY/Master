module PAFPAFPAF8Monades where

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map


{-{

PAF 8 : Programmation monadiques
================================

**Plan du cours** :

1) Combiner des actions contextuelles

2) La typeclasse Monad : exemple de MMaybe

3) Les lois des monades

}-}


{-{

1) Combiner des actions contextuelles
-------------------------------------

Exemple 1 : composer des calculs optionnels
---------

}-}

newtype StudentId = StudentId String
  deriving (Show, Eq, Ord)

newtype Email = Email String
  deriving (Show, Eq)

emailsDB :: Map StudentId Email
emailsDB = Map.fromList [ (StudentId "321001", Email "pb@toto.net")
                          , (StudentId "321042", Email "en@tuti.fr")]


loginsDB :: Map String StudentId
loginsDB = Map.fromList [ ("pabe", StudentId "321001")
                      , ("enou", StudentId "321042")
                      , ("tutu", StudentId "432312") ]


emailFromLogin :: String -> Maybe Email
emailFromLogin login =
  case Map.lookup login loginsDB of
    Nothing -> Nothing
    Just id -> Map.lookup id emailsDB

-- >>> emailFromLogin "pabe"
-- Just (Email "pb@toto.net")

-- >>> emailFromLogin "toto"
-- Nothing

-- >>> emailFromLogin "tutu"
-- Nothing

{-

Cette fonction ne s�pare pas bien les pr�occupations
(separation of concerns).

(1) Il y a d'un c�t� : chercher un id � partir d'un login

et (2) de l'autre c�t� : chercher un email � partir d'un id

-}

fetchId :: String -> Maybe StudentId
fetchId login = Map.lookup login loginsDB

fetchEmail :: StudentId -> Maybe Email
fetchEmail id = Map.lookup id emailsDB

{-

Ce que l'on voudrait faire, c'est "combiner" ou "cha�ner"
s�quentiellement les deux op�rations pour obtenir
(une meilleure impl�mentation de) emailFromLogin

-}

-- l'op�rateur de composition "normale"   (hors-contexte)
-- ne fonctionne pas.

-- >>> :t (.)
-- (.) :: (b              -> c          ) -> (a        -> b ) -> a -> c

--        (StudentId <>b  -> Maybe Email) -> (a=String -> b=Maybe StudentId) 

-- >>> :t fetchEmail . fetchId
-- <interactive>:1:14-20: error:
--     * Couldn't match type `Maybe StudentId' with `StudentId'
--       Expected type: String -> StudentId
--         Actual type: String -> Maybe StudentId
--     * In the second argument of `(.)', namely `fetchId'
--       In the expression: fetchEmail . fetchId


-- >>> :t fetchEmail
-- fetchEmail :: StudentId -> Maybe Email

combine1 :: Maybe StudentId -> (StudentId -> Maybe Email) -> Maybe Email
combine1 Nothing _ = Nothing
combine1 (Just sid) f = f sid

-- >>> (fetchId "pabe") `combine1` fetchEmail
-- Just (Email "pb@toto.net")

-- >>> (fetchId "toto") `combine1` fetchEmail
-- Nothing

-- >>> (fetchId "tutu") `combine1` fetchEmail
-- Nothing


combine :: Maybe a -> (a -> Maybe b) -> Maybe b
combine Nothing _ = Nothing
combine (Just sid) f = f sid


-- >>> (fetchId "pabe") `combine` fetchEmail
-- Just (Email "pb@toto.net")

-- >>> (fetchId "toto") `combine` fetchEmail
-- Nothing

-- >>> (fetchId "tutu") `combine` fetchEmail
-- Nothing

-- Une meilleure version de notre fonction :
emailFromLoginV2 :: String -> Maybe Email
emailFromLoginV2 login = (fetchId login) `combine` fetchEmail

-- >>> emailFromLoginV2 "pabe"
-- Just (Email "pb@toto.net")

-- >>> emailFromLoginV2 "toto"
-- Nothing

-- >>> emailFromLoginV2 "tutu"
-- Nothing

{-

En fait, la fonction combine existe d�j�, de fa�on encore
plus g�n�rique.    Cela correspond au concept de "bind" (monadique)
qui se nomme en Haskell :  >>=

-}

-- >>> :t (>>=)
-- (>>=)    :: Monad m => m     a -> (a -> m     b) -> m     b
-- combine  ::            Maybe a -> (a -> Maybe b) -> Maybe b

-- (du coup, on peut penser que m=Maybe  fonctionne ...)

-- >>> (fetchId "pabe") >>= fetchEmail
-- Just (Email "pb@toto.net")

-- >>> (fetchId "toto") >>= fetchEmail
-- Nothing

-- >>> (fetchId "tutu") >>= fetchEmail
-- Nothing

-- On obtient le m�me comportement, et on verra pr�cis�ment
-- pourquoi dans la deuxi�me partie du cours.

-- Donc cette version est plus "haskelienne"
emailFromLoginV3 :: String -> Maybe Email
emailFromLoginV3 login = (fetchId login) >>= fetchEmail

{-

Exemple 2 : les calculs non-d�terministes
---------

-}

finRange :: Int -> Int -> [Int]
finRange m n | m < n = m : finRange (m+1) n
             | otherwise = []


-- >>> finRange 2 6
-- [2,3,4,5]

listCombine :: [a] -> (a -> [b]) -> [b]
listCombine [] _ = []
listCombine (x:xs) f = (f x) <> (listCombine xs f)

-- >>> (finRange 1 8) `listCombine` (\x -> [(x * x)])
-- [1,4,9,16,25,36,49]

-- >>> (finRange 1 5) `listCombine` (\x -> ['A', 'B'] `listCombine` (\y -> [(x, y)]))
-- [(1,'A'),(1,'B'),(2,'A'),(2,'B'),(3,'A'),(3,'B'),(4,'A'),(4,'B')]

-- On est en train de faire, avec `listCombine`, des compr�hensions de listes

-- >>> [(x,y) | x <- (finRange 1 5), y <- ['A', 'B']]
-- [(1,'A'),(1,'B'),(2,'A'),(2,'B'),(3,'A'),(3,'B'),(4,'A'),(4,'B')]

-- Et en fait, le listCombine est simplement une impl�mentation du bind
-- pour les listes (en Haskell)

-- >>> :t (>>=)
-- (>>=)        :: Monad m => m     a -> (a -> m     b) -> m     b
-- combine      ::            Maybe a -> (a -> Maybe b) -> Maybe b
-- listCombine  ::                 [a]-> (a ->      [b])->      [b]

-- Clairement, pour le typage, listCombine est une "sp�cialisation" du type
-- de  bind  (>>=)   pour m=[]

-- >>> (finRange 1 8) >>= (\x -> [(x * x)])
-- [1,4,9,16,25,36,49]

-- >>> (finRange 1 5) >>= (\x -> ['A', 'B'] >>= (\y -> [(x, y)]))
-- [(1,'A'),(1,'B'),(2,'A'),(2,'B'),(3,'A'),(3,'B'),(4,'A'),(4,'B')]

{-

Exemple 3 : les entr�es/sorties (IO)
---------

On prend deux fonctions dans IO :

-- >>> :t getLine
-- getLine :: IO String

-- >>> :t putStrLn
-- putStrLn :: String -> IO ()

On voudrait combiner ces deux op�rations...

-}

ioCombine1 :: IO String -> (String -> IO ()) -> IO ()
ioCombine1 act f = do
  str <- act  -- ici, str est du type String
  f str

echoV1 :: IO ()
echoV1 = getLine `ioCombine1` putStrLn


-- On peut g�n�raliser le type :


ioCombine :: IO a -> (a -> IO b) -> IO b
ioCombine act f = do
  val <- act  -- ici, val est du type ???
  f val

echoV2 :: IO ()
echoV2 = getLine `ioCombine` putStrLn

-- Et on peut encore remarquer que ioCombine est une "sp�cialisation"
-- de bind

-- >>> :t (>>=)
-- (>>=)        :: Monad m => m     a -> (a -> m     b) -> m     b
-- combine      ::            Maybe a -> (a -> Maybe b) -> Maybe b
-- listCombine  ::                 [a]-> (a ->      [b])->      [b]
-- ioCombine    ::               IO a -> (a ->    IO b) ->    IO b

-- Donc le contexte m=IO a l'air de "marcher" ...

echoV3 :: IO ()
echoV3 = getLine >>= putStrLn

-- A retenir : on a trois contextes diff�rents et � chaque fois
-- une m�me solution pour r�soudre le probl�me de combinaison
-- valeur contextuelle  >>=  fonction hors-context -> retour contextuel
-- et cette solution se nomme >>= (bind)

{-

2)  La typeclasse Monad  (et l'impl�mentation du bind)
=======================

-- >>> :info Monad
-- type Monad :: (* -> *) -> Constraint
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b

-}

data MMaybe a = NNothing | JJust a
  deriving Show

instance Functor MMaybe where
  fmap _ NNothing = NNothing
  fmap g (JJust x) = JJust (g x)

instance Applicative MMaybe where
  pure = JJust
  (JJust g) <*> (JJust x) = JJust (g x)
  _ <*> _ = NNothing


combineMMaybe :: MMaybe a -> (a -> MMaybe b) -> MMaybe b
combineMMaybe NNothing _ = NNothing
combineMMaybe (JJust x) f = f x

instance Monad MMaybe where
  -- (>>=) : MMaybe a -> (a -> MMaybe b) -> MMaybe b
  (>>=) = combineMMaybe


-- >>> (JJust 42) >>= (\x -> JJust (x+1))
-- JJust 43

-- >>> (JJust 42) >>= (\x -> JJust (x+1) >>= (\y -> JJust (even y)))
-- JJust False

-- >>> (JJust 42) >>= (\x -> JJust (x+2) >>= (\y -> JJust (even y)))
-- JJust True

-- >>> (JJust 42) >>= (\x -> NNothing >>= (\y -> JJust (even y)))
-- NNothing

{-

A propos de la notation do
--------------------------

Il est clair que l'utilisation directe du bind  (>>=)  n'est pas
tr�s agr�able...

Donc Haskell introduit un sucre syntaxique : la notation do
qu'on utilise beaucoup dans la monade IO, mais qui est disponible
pour tout contexte monadique  (tout m  instance de la typeclasse Monad).

-}

-- un premier exemple dans IO

fact :: Integer -> Integer
fact n = aux n 1
  where aux n acc | n > 0 = aux (n-1) (n*acc)
                  | otherwise = acc

computeFact1 :: IO ()
computeFact1 =
  putStr "n = "
  >> getLine
  >>= (\s -> let n = read s :: Integer
             in (putStr ("La factorielle de " <> (show n) <> " est: ")
                 >> (putStrLn (show (fact n)))))

-- on va essayer de rendre ce programme plus �l�gant

computeFact :: IO ()
computeFact = do
  putStr "n = "
  s <- getLine
  let n = read s :: Integer  -- pour une version safe, cf.  `reads`
  putStr ("La factorielle de " <> (show n) <> " est: ")
  putStrLn (show (fact n))

-- et donc �a marche pour d'autres contextes monadiques

-- >>> (Just 42) >>= (\x -> pure (x+1)) >>= (\y -> pure (even y))

plusJoli :: Integer -> Maybe Bool
plusJoli n = do
  x <- pure n
  y <- pure (x + 1)   -- ou pure  (rappel : pure = return)
  return (even y)

-- >>> plusJoli 42
-- Just False

-- >>> plusJoli 43
-- Just True


-- encore un exemple :

-- emailFromLoginV3 :: String -> Maybe Email
-- emailFromLoginV3 login = (fetchId login) >>= fetchEmail

emailFromLoginV4 :: String -> Maybe Email
emailFromLoginV4 login = do
  id <- fetchId login
  fetchEmail id

{-

Voici les r�gles de transformation du  "do vers bind"
(un peu simplifi�e, cf. manuel de GHC)

R�gle 1 : (expression hors-contexte)

do <expr-pure> => <expr-pure>

R�gle 2 : (sequence)

do { <expr> ; <nexts> }  =>  <expr> >> do { <nexts> }

do              =>  <expr> >> do
  <expr>                         <next1>  
  <next1>                        <next2>
  <next2>                        ...
  ...                            <nextN>
  <nextN>

R�gle 3 : (bind)

do { v <- <expr> ; <nexts>}   =>   <expr> >>= (\v -> do { <nexts> } )

R�gle 4 : (let)

do { let x = <expr> ; <nexts> } => let x = <expr> in do { <nexts> }

-}


{-

Remarque :

do
  act1
  act2
  ...
  actN

peut s'�crire :

do { act1 ; act2 ; ... ; actN }


-}

-- Un dernier exemple sur les listes :

-- >>> (finRange 1 8) >>= (\x -> pure (x * x))
-- [1,4,9,16,25,36,49]

-- >>> do { x <- finRange 1 8 ; pure (x * x) } 
-- [1,4,9,16,25,36,49]

-- >>> pure 42  :: [Integer]
-- [42]

-- >>> (finRange 1 5) `listCombine` (\x -> ['A', 'B'] `listCombine` (\y -> pure (x, y)))
-- [(1,'A'),(1,'B'),(2,'A'),(2,'B'),(3,'A'),(3,'B'),(4,'A'),(4,'B')]


-- >>> (finRange 1 5) >>= (\x -> ['A', 'B'] >>= (\y -> pure (x, y)))
-- [(1,'A'),(1,'B'),(2,'A'),(2,'B'),(3,'A'),(3,'B'),(4,'A'),(4,'B')]

-- >>> do { x <- finRange 1 5 ; y <- ['A', 'B'] ; return (x, y) }
-- [(1,'A'),(1,'B'),(2,'A'),(2,'B'),(3,'A'),(3,'B'),(4,'A'),(4,'B')]

-- >>> [(x,y) | x <- finRange 1 5, y <- ['A', 'B']]
-- [(1,'A'),(1,'B'),(2,'A'),(2,'B'),(3,'A'),(3,'B'),(4,'A'),(4,'B')]


{-

3) Les lois monadiques
----------------------

Le bind >>=  est plus une forme de cha�nage "s�quentiel" qu'une composition
proprement dite.

Question :  A quoi ressemblerait la composition dans un contexte monadique
--------

La composition hors-contexte (ou pure, au sens de purement fonctionnel)

-- >>> :t (.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c

On cherche plut�t quelque chose de la forme :

Monad m => (b -> m c) -> (a -> m b) -> a -> m c

Ca c'est la composition droite-gauche,  on peut aussi
introduire la composition gauche-droite

Monad m => (a -> m b) -> (b -> m c) -> a -> m c

-}

-- Dans Hoogle, on a trouv� :

-- >>> :t (<=<)
-- (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c

-- et :

-- >>> :t (>=>)
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c

{-

Ces op�rateurs s'appelles les "poissons" (fish left) et (fish right ou fish)
et ce sont les op�rateurs de composition monadique.

Voici un exemple d'utilisation :

On se rappelle nos deux fonctions de "fetch"

-- >>> :t fetchId
-- fetchId :: String -> Maybe StudentId

-- >>> :t fetchEmail
-- fetchEmail :: StudentId -> Maybe Email

-}

-- si on pose a=String et m=Maybe et c=Email,  et b=StudentId

emailFromLoginV5 :: String -> Maybe Email
emailFromLoginV5 = fetchEmail <=< fetchId

-- >>> emailFromLoginV5 "pabe"
-- Just (Email "pb@toto.net")

-- Sans doute la plus "jolie" version :
emailFromLoginV6 :: String -> Maybe Email
emailFromLoginV6 = fetchId >=> fetchEmail

-- >>> emailFromLoginV6 "pabe"
-- Just (Email "pb@toto.net")

{-

Les lois monadiques sont les lois des cat�gories
(expliqu�es au cours 6 ?)

Pour une monade m, les objets a, b, etc. sont des types
et les fl�ches (morphismes) sont des fonctions de type :  a -> m b
et l'op�rateur de composition est >=>   (ou <=<)

Lois d'identit� :

 (1) pure >=> f = f
 (2) f >=> pure = f

Loi d'associativit� pour la composition (>=>) :

 (3) (f >=> g) >=> h  =   f >=> (g >=> h)


Exercice : transcrire ces r�gles avec le bind >>=

-}



