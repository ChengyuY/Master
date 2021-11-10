module Validation where

import AdressBook

import Data.Char
import Text.Regex.Applicative

newtype Validation e a = Validation (Either e a)
  deriving (Show, Eq)

mapVal :: (a -> b) -> (Validation e a) -> (Validation e b) 
mapVal f (Validation ei) = Validation (fmap f ei) 
                                 
instance Functor (Validation e) where
  fmap = mapVal

pureVal :: a -> Validation e a
pureVal = Validation . Right

applyVal :: Monoid e => Validation e (a -> b) -> Validation e a -> Validation e b
applyVal (Validation (Left er)) (Validation (Right x)) =  Validation (Left er)
applyVal (Validation (Right f)) (Validation (Left er)) = Validation (Left er)
applyVal (Validation (Right f)) (Validation (Right x)) = Validation (Right (f x))
applyVal (Validation (Left er1)) (Validation (Left er2)) = Validation (Left (er1 <> er2))

instance Monoid e => Applicative (Validation e) where
  pure = pureVal
  (<*>) = applyVal

type Error = String
type Valid a = Validation [Error] a

invalid :: String -> Valid a
invalid msg = Validation (Left [msg])

valid :: a -> Validation e a
valid v = Validation (Right v)

validateNonEmpty :: String -> Valid ()
validateNonEmpty [] = invalid "La chaine est vide"
validateNonEmpty xs = valid ()

validateMinuscule :: Char -> Valid Char
validateMinuscule ch | (ch >= 'a' && ch <= 'z') = valid ch
                     | otherwise = invalid ("Ce n'est pas une miniscule : "  <> (show ch))


validateMajuscule :: Char -> Valid Char
validateMajuscule ch | (ch >= 'A' && ch <= 'Z') = valid ch
                     | otherwise = invalid ("Ce n'est pas une majuscule : "  <> (show ch))

validateNom :: String -> Valid String
validateNom str = (:) <$> (vNE *> vH) <*> vT
    where vNE = validateNonEmpty str
          vH = validateMajuscule (head str)
          vT = case str of
              [] -> Validation $ Right []
              c:cs -> traverse validateMinuscule cs

validatePrenom :: String -> Valid String
validatePrenom = validateNom

-- on utilise ci-dessous la bibliothèque `regex-applicative`
-- qui permet d'encoder des expressions rationnelles en
-- style applicatif, cf. Défi 1.

charRE :: RE Char Char
charRE = psym (\x -> (isAlphaNum x) || (x == '.'))

list :: a -> [a]
list v = [v]

ssym :: Char -> RE Char String
ssym ch = list <$> (sym ch)

emailRE :: RE Char Email
emailRE = Email <$> (some charRE) <*> ((ssym '@') *> (some charRE))

validateEmail :: String -> Valid Email
validateEmail txt =
  case match emailRE txt of
    Nothing -> invalid ("Adresse email erronee: " <> txt)
    Just email -> valid email

digitRE :: RE Char String
digitRE = list <$> psym isDigit 

twoDigitsRE :: RE Char String
twoDigitsRE = (<>) <$> digitRE <*> digitRE

telephoneRE :: RE Char Telephone
telephoneRE = Telephone <$> twoDigitsRE
              <*> (ssym '.' *> twoDigitsRE)
              <*> (ssym '.' *> twoDigitsRE)
              <*> (ssym '.' *> twoDigitsRE)
              <*> (ssym '.' *> twoDigitsRE)

validateTel :: String -> Valid Telephone
validateTel txt =
  case match telephoneRE txt of
    Nothing -> invalid ("Numero de telephone mal-forme: " <> txt)
    Just tel -> valid tel

validateContact :: String -> String -> String -> String -> Valid Contact
validateContact nom prenom email tel =
  Contact <$> (validateNom nom) <*> (validatePrenom prenom) <*> (validateEmail email) <*> (validateTel tel)
