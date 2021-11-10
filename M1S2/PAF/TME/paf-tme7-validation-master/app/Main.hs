module Main where

import System.IO (hFlush, stdout)

import Data.List (foldl')

import Data.Sequence (Seq (..))
import qualified Data.Sequence as S

import AdressBook
import Validation

data Action = AddContact | RemoveContact | ListContacts | QuitApp | WrongAction

dispatch :: String -> Action
dispatch "1" = AddContact
dispatch "2" = RemoveContact
dispatch "3" = ListContacts
dispatch "4" = QuitApp
dispatch _ = WrongAction

menu :: IO Action
menu = do
  putStrLn ""
  putStrLn "Menu :"
  putStrLn "(1) Ajouter un contact"
  putStrLn "(2) Retirer un contact"
  putStrLn "(3) Lister les contacts"
  putStrLn "(4) Quitter l'application"
  putStrLn "----"
  putStr "Choix : "
  hFlush stdout
  choix <- getLine
  return (dispatch choix)


askPart :: String -> (String -> Valid b) -> IO b
askPart part validate = do
  putStr $ part ++ " : "
  hFlush stdout
  str <- getLine
  case (validate str) of
    Validation (Left errs) -> do putStrLn ("Erreur : " ++ (foldl' (++) "" errs))
                                 askPart part validate
    Validation (Right x) -> return x

addContact :: AdressBook -> IO AdressBook
addContact (AdressBook contacts) = do
  nom <- askPart "Nom" validateNom
  prenom <- askPart "Prenom" validatePrenom
  email <- askPart "Email" validateEmail
  tel <- askPart "Tel." validateTel
  return (AdressBook (contacts :|> (Contact nom prenom email tel)))

delContact :: AdressBook -> IO AdressBook
delContact book@(AdressBook contacts) = do
  putStrLn $ "Il y a " ++ (show (S.length contacts)) ++ " contacts"
  putStr $ "Quel contact voulez-vous retirer (1-" ++ (show (S.length contacts)) ++ ") ? "
  hFlush stdout
  r <- getLine
  let [(nb,rest)] = reads r :: [(Int, String)]
  if rest == r
  then do putStrLn ".. il faut saisir un entier!"
          delContact book
  else if (nb < 1) || (nb > S.length contacts)
       then do putStrLn ".. il faut saisir un entier dans le bon intervalle!"
               delContact book
       else do putStrLn $ ".. suppression du contact #" ++ (show nb)
               return (AdressBook (S.deleteAt (nb-1) contacts))

loop :: AdressBook -> IO ()
loop book = do
  act <- menu
  case act of
    AddContact -> do putStrLn "\nAjout d'un contact"
                     book' <- addContact book
                     loop book'
    RemoveContact -> do putStrLn "\nRetrait d'un contact"
                        book' <- delContact book
                        loop book'
    ListContacts -> do putStrLn "\nListe des contacts :"
                       putStrLn (presentBook book)
                       loop book
    QuitApp -> putStrLn "Bye bye"
    _ -> do putStrLn ".. je ne comprends pas ce choix, pourtant ce n'est pas compliqué !"
            loop book
            
main :: IO ()
main = do
  putStrLn ""
  putStrLn "AdressBook v0.1 (C) 2020 (PIF) PAF (POUF)"
  putStrLn "=========="
  putStrLn ""
  loop (AdressBook S.empty)
