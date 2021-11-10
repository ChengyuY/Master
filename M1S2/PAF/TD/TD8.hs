module Lib where

import qualified Data.Map.Strict as M
import Control.Monad

pokedex :: M.Map Int String
pokedex = M.fromList [(1, "Flammignon"), 
                         (2, "Aquasaucisse"), 
                         (3, "Plantenpot"), 
                         (4, "Sacavin"),
                         (18, "Hippopobraise"),
                         (42, "Océanguez"),
                         (124, "Mammouthenfer")]

evolutions :: M.Map Int Int
evolutions = M.fromList [(1 ,18), (2, 42), (18, 124)]

trouvePoke :: Int -> Maybe String
trouvePoke = flip M.lookup pokedex

trouveEvo :: Int -> Maybe Int
trouveEvo = flip M.lookup evolutions

trouvePokevoevo :: Int -> Maybe String 
trouvePokevoevo n = case trouveEvo n of
                        Nothing -> Nothing 
                        Just n2 -> case trouveEvo n2 of
                                        Nothing -> Nothing 
                                        Just n3 -> trouvePoke n3


trouvePokevoevo2 :: Int -> Maybe String 
trouvePokevoevo2 n = trouveEvo n >>= trouveEvo >>= trouvePoke

trouvePokevoevo3 :: Int -> Maybe String 
trouvePokevoevo3 = trouveEvo >=> trouveEvo >=> trouvePoke


-- >>> fmap trouvePokevoevo [1, 2, 18, 124, 3, 5]
-- [Just "Mammouthenfer",Nothing,Nothing,Nothing,Nothing,Nothing]
-- >>> fmap trouvePokevoevo2 [1, 2, 18, 124, 3, 5]
-- [Just "Mammouthenfer",Nothing,Nothing,Nothing,Nothing,Nothing]

-- >>> fmap trouvePokevoevo3 [1, 2, 18, 124, 3, 5]
-- [Just "Mammouthenfer",Nothing,Nothing,Nothing,Nothing,Nothing]


-- >>> trouvePoke 3
-- Just "Plantenpot"
-- >>> trouvePoke 5
-- Nothing

-- >>> trouveEvo 1
-- Just 18
-- >>> trouveEvo 18
-- Just 124
-- >>> trouvePoke 124
-- Just "Mammouthenfer"
-- >>> trouvePoke (trouveEvo (trouveEvo 1))
-- Couldn't match expected type ‘Int’ with actual type ‘Maybe Int’
-- Couldn't match expected type ‘Int’ with actual type ‘Maybe Int’
-- >>> (trouveEvo 1) >>= trouveEvo >>= trouvePoke
-- Just "Mammouthenfer"
-- >>> (trouveEvo 3) >>= trouveEvo >>= trouvePoke
-- Nothing
-- >>> (trouveEvo 2) >>= trouveEvo >>= trouvePoke
-- Nothing

-- >>> trouveEvo >=> trouvePoke $ 1
-- Just "Hippopobraise"
--- >>> trouveEvo >=> trouvePoke $ 43
-- Nothing
--- >>> trouveEvo >=> trouvePoke $ 124
-- Nothing

prop_identiteGauche_loi :: (Monad m, Eq (m b)) => (a -> m b) -> a -> Bool
prop_identiteGauche_loi f e = (pure >=> f) e == f e
prop_identiteDroite_loi :: (Monad m, Eq (m b)) => (a -> m b) -> a -> Bool
prop_identiteDroite_loi f e = (f >=> pure) e == f e
-- >>> fmap (prop_identiteGauche_loi trouvePoke) [1, 2, 4, 5, 42, 43]
-- [True,True,True,True,True,True]
-- >>> fmap (prop_identiteDroite_loi trouvePoke) [1, 2, 4, 5, 42, 43]
-- [True,True,True,True,True,True]
prop_associativite_loi :: (Monad m, Eq (m d)) => (a -> m b) -> (b -> m c) -> (c -> m d) -> a -> Bool
prop_associativite_loi f g h e = ((f >=> g) >=> h) e == (f >=> (g >=> h)) e
-- >>> fmap (prop_associativite_loi trouveEvo trouveEvo trouvePoke) [1, 2, 4, 5, 42, 43]
-- [True,True,True,True,True,True]

poissonD :: (Monad m) => (a -> m b) -> (b -> m c) -> (a -> m c)
poissonD f g e = f e >>= g

prop_poissonD :: (Monad m, Eq (m c)) => (a -> m b) -> (b -> m c) -> a -> Bool
prop_poissonD f g e = (f >=> g) e == (f `poissonD` g) e
-- >>> fmap (prop_poissonD trouveEvo trouvePoke) [1, 2, 4, 5, 42, 43]
-- [True,True,True,True,True,True]

prop_identiteGauche2_loi :: (Monad m, Eq (m b)) => (a -> m b) -> a -> Bool
prop_identiteGauche2_loi f e = (pure e >>= f) == f e
prop_identiteDroite2_loi :: (Monad m, Eq (m b)) => (a -> m b) -> a -> Bool
prop_identiteDroite2_loi f e = (f e >>= pure) == f e
prop_associativite2_loi :: (Monad m, Eq (m d)) => (a -> m b) -> (b -> m c) -> (c -> m d) -> a -> Bool
prop_associativite2_loi f g h e = (((\z -> f z >>= g) e) >>= h) == ((f e) >>= (\y -> (g y) >>= h))
-- >>> fmap (prop_identiteGauche2_loi trouvePoke) [1, 2, 4, 5, 42, 43]
-- [True,True,True,True,True,True]
-- >>> fmap (prop_identiteDroite2_loi trouvePoke) [1, 2, 4, 5, 42, 43]
-- [True,True,True,True,True,True]
-- >>> fmap (prop_associativite2_loi trouveEvo trouveEvo trouvePoke) [1, 2, 4, 5, 42, 43]
-- [True,True,True,True,True,True]

newtype Identite a = Id { ouvreId :: a }
    deriving (Show, Eq)

instance Functor Identite where
    fmap g (Id x) = Id (g x)
instance Applicative Identite where
    pure = Id
    (Id g) <*> (Id x) = Id (g x)
instance Monad Identite where
    (Id x) >>= f = f x



-- (Inversion) v == Id (ouvreId v)

-- *** Monade ***

type Pile a = [a]

pop :: Pile a -> (a, Pile a)
pop [] = error "Pile Vide"
pop (x:xs) = (x, xs)

push :: a -> Pile a -> ((), Pile a)
push x xs = ((), x:xs)

-- >>> (pop . push 3 . push 4) []
-- Couldn't match type ‘((), Pile a1)’ with ‘[a0]’
-- Expected type: Pile a2 -> Pile a0
--   Actual type: Pile a2 -> ((), Pile a1)
-- Couldn't match type ‘((), Pile a2)’ with ‘[a1]’
-- Expected type: Pile a2 -> Pile a1
--   Actual type: Pile a2 -> ((), Pile a2)
-- >>> (\(_,x) -> push 3 x) (push 4 [])
-- ((),[3,4])
-- >>> (\(_,x) -> pop x) $ (\(_,x) -> push 3 x) (push 4 [])
-- (3,[4])

newtype Compteur = C Integer deriving Show 

incr :: Compteur -> (Integer, Compteur)
incr (C c) = (c, C (c+1))

decr :: Compteur -> (Integer, Compteur)
decr (C c) = (c, C (c-1))

data Memoire a = MM {pile :: Pile a, 
                     compteur :: Compteur} deriving Show

mpop :: Memoire a -> (a, Memoire a)
mpop (MM p c) = let (v, np) = pop p in (v, MM np c) 

mpush :: a -> Memoire a -> ((), Memoire a)
mpush e (MM p c) = let (v, np) = push e p in (v, MM np c) 

mincr :: Memoire a -> (Integer, Memoire a)
mincr (MM p c) = let (v, nc) = incr c in (v, MM p nc) 

mdecr :: Memoire a -> (Integer, Memoire a)
mdecr (MM p c) = let (v, nc) = decr c in (v, MM p nc) 

newtype Etat s a = E { run :: (s -> (a, s)) }

instance Functor (Etat s) where
    -- (a -> b) -> (Etat s a) -> (Etat s b)
    fmap f (E calcul) = E (\e -> let (res, ne) = calcul e in (f res, ne))
instance Applicative (Etat s) where
    -- a -> Etat s a
    pure v = E (\e -> (v, e))
    -- Etat s (a -> b) -> Etat s a -> Etat s b
    (<*>) (E calculf) (E calcula) = E (\e -> let (resf, ne) = calculf e in 
                                             let (resa, nne) = calcula ne in
                                                 (resf resa, nne))
-- (<*>) (E calculf) (E calcula) = E (\e -> let (resa, ne) = calcula e in 
--                                             let (resf, nne) = calculf e in
--                                                 (resf resa, nne / ne)) => FAUX
-- (<*>) (E calculf) (E calcula) = E (\e -> let (resa, ne) = calcula e in 
--                                             let (resf, nne) = calculf ne in
--                                                 (resf resa, nne)) => Juste
instance Monad (Etat s) where
    -- Etat s a -> (a -> Etat s b) -> Etat s b
    (>>=) (E calcula) f = E (\e -> let (resa, ne) = calcula e in
                                   let E calculr = f resa in
                                    calculr ne)
--(>>=) (E calcula) f = E (\e -> let (resa, ne) = calcula e in
--                                   ((etat (f resa)) ne)


type EtatP a b = Etat (Pile a) b
type EtatC b = Etat Compteur b
type EtatM a b = Etat (Memoire a) b

pops :: EtatP a a -- Etat (Pile a) a -.-  (Pile a -> (a, Pile a))
pops = E pop

pushs :: a -> EtatP a () -- Etat (Pile a) () - - -  Pile a -> ((), Pile a)
pushs = E . push 
-- pushs e = E (push e)

incrs :: EtatC Integer -- Etat Compteur Integer 
incrs = E incr

decrs :: EtatC Integer 
decrs = E decr

mpushs :: a -> EtatM a () -- Etat (Memoire a) ()  - -- - Memoire a -> ((), Memoire a)
mpushs = E . mpush 

mpops :: EtatM a a
mpops = E mpop

mincrs :: EtatM a Integer
mincrs = E mincr

mdecrs :: EtatM a Integer
mdecrs = E mdecr


stackManip :: EtatP Int Int
stackManip = do
                pushs 1 -- EtatP Int
                pushs 2
                pushs 3
                x <- pops
                return x

-- >>> (run stackManip) []
-- (3,[2,1])
-- >>> (run stackManip) [3, 3]
-- (3,[2,1,3,3])

stackManip2 :: EtatP Int Int
stackManip2 = do
                pushs 1 -- EtatP Int
                pushs 2
                y <- pops
                pushs 3
                x <- pops
                return (x + y)


-- >>> (run stackManip2) []
-- (5,[1])²


counterManip :: EtatC Integer
counterManip = do
                incrs
                incrs
                incrs
                x <- decrs
                return x

-- >>> (run counterManip) (C 0) 
-- (3,C 2)

-- >>> (run counterManip) (C 42) 
-- (45,C 44)

memManip :: EtatM Integer (Integer, Integer)
memManip = do 
                mpushs 1 -- EtatP Int
                mpushs 2
                y <- mpops
                mpushs 3
                x <- mpops
                z <- return (x + y)
                mincrs
                mincrs
                mincrs
                x <- mdecrs
                return (z, x)

-- >>> (run memManip) (MM [] (C 0))
-- ((5,3),MM {pile = [1], compteur = C 2})

memManip2 :: EtatM Integer ()
memManip2 = (mpushs 1) >>= (\_ -> mpushs 2 >>= (\_ -> mpops >>= (\y -> mpushs 3))) -- ...