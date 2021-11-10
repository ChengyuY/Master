module Main where

import Data.Char (toLower)
import System.Exit (exitSuccess)

import Control.Monad (forever)
import Data.Traversable (for)
import System.IO (hFlush, stdout)
import System.Random

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import MindEngine


banner :: String
banner = "\n\
\MrMind v0.1 -- Copyright (C) 2019 F. Peschanski\n\
\===========\n\
\"

mainMenuStr :: String
mainMenuStr = "\
\What would you like to do?\n\
\\n - (P)lay the codebreaker against the (brilliant!) MrMind codemaker?\n\
\  - Play the codebreaker against a (H)uman codemaker?\n\
\  - Be the (C)odemaker and see the (neat) MrMind codebreaker?\n\
\  - Just (R)elax and see MrMind play both the codemaker and the codebreaker?\n\
\  - E(X)it MrMind?\n\n\
\==> Please type one letter (P,H,C,R,X) then `enter`"

data Role = Human | Computer

data MainCommand = Play { maker :: Role, breaker :: Role }
  | ExitCmd
  | DoesNotUnderstandCmd

parseMainCmd :: String -> MainCommand
parseMainCmd cmd = if length cmd <= 2
                   then case toLower (head cmd) of
                          'p' -> Play Computer Human
                          'h' -> Play Human Human
                          'c' -> Play Human Computer
                          'r' -> Play Computer Computer
                          'x' -> ExitCmd
                          _ -> DoesNotUnderstandCmd
                   else DoesNotUnderstandCmd

mainMenu :: IO MainCommand
mainMenu = do
  putStrLn mainMenuStr
  putStr "> "
  hFlush stdout
  inp <- getLine
  return $ parseMainCmd inp
  

main :: IO ()
main = do
  putStrLn banner
  mainLoop

mainLoop :: IO ()
mainLoop = forever $ do
  cmd <- mainMenu
  case cmd of
    ExitCmd -> do
       putStrLn "Bye bye!"
       exitSuccess
    DoesNotUnderstandCmd ->
      do putStrLn "<<<I do not understand the command>>>\n"
    Play maker breaker -> gameLoop maker breaker
    _ -> putStrLn "Not yet implemented"


gameLoop :: Role -> Role -> IO ()
gameLoop maker breaker = do
  putStrLn "The game starts ..."
  putStrLn "------------------"
  putStrLn "First the maker must create its secret code"
  case maker of
    Human -> putStrLn " ==> please summon the human codemaker"
    Computer -> putStrLn "==> the computer codemaker has been summoned"
  psec <- prepareSecret maker 
  case psec of
    Nothing -> putStrLn "==> secret is incorrect (abort)"
    Just sec -> do
      putStrLn "==> secret is ready"
      guessLoop sec breaker 1

prepareSecret :: Role -> IO (Maybe Secret)
prepareSecret Computer = return Nothing
{-  rndCode <- randomCodestring
  return mkSecret (foldr (\c sec -> sec :>| (letterToColor c)) Seq.empty  -}

prepareSecret Human = do
  putStrLn "------------------"
  putStrLn "\
\Prepare a secret code string using the following characters:\n\
\  - 'k' for Black, 'b' for Blue, 'g' for Green, 'y' for Yellow,\n\
\    'c' for Cyan (light blue), 'w' for White, 'm' for Magenta, 'r' for Red\n\
\  e.g. \"yycg\" is Yellow-Yellow-Cyan-Green"
  putStr "> "
  hFlush stdout
  inp <- getLine
  case lettersToColor inp of
    Nothing -> do
      putStrLn "<<<Wrong input>>>"
      prepareSecret Human
    Just sec -> do
      putStr $ replicate 30 '\n'
      return $ Just (mkSecret sec)

letterToColor :: Char -> Maybe Peg
letterToColor c = case f (toLower c) of
                    PEmpty -> Nothing
                    col -> Just col
  where f 'k' = Black
        f 'b' = Blue
        f 'g' = Green
        f 'y' = Yellow
        f 'c' = Cyan
        f 'w' = White
        f 'm' = Magenta
        f 'r' = Red
        f _ = PEmpty

lettersToColor :: String -> Maybe (Seq Peg)
lettersToColor str = fmap Seq.fromList $ for str letterToColor  


guessLoop :: Secret -> Role -> Int -> IO ()
guessLoop sec Computer n = undefined

guessLoop sec Human n = do
  putStrLn "------------------"
  putStr ("Please guess the secret code" <> "(round #" <> (show n) <> ")")
  putStrLn "\
\ using the following characters:\n\
\  - 'k' for Black, 'b' for Blue, 'g' for Green, 'y' for Yellow,\n\
\    'c' for Cyan (light blue), 'w' for White, 'm' for Magenta, 'r' for Red\n\
\  e.g. \"yycg\" is Yellow-Yellow-Cyan-Green"
  putStr "> "
  hFlush stdout
  inp <- getLine
  case lettersToColor inp of
    Nothing -> do
      putStrLn "<<<bad input>>>\n"
      guessLoop sec Human n
    Just guess ->
      if wrongGuess sec guess
      then do
        putStrLn "<<<wrong guess size>>>\n"
        guessLoop sec Human n
      else do
      putStrLn "... checking your guess"
      case verify sec guess of
        ans@(Answer cor pos) ->
          if winning sec ans
          then do
            putStrLn $ "==> You Won! (in " <> (show n) <> " rounds)"
          else do
            putStrLn $ "==> " <> (show cor) <> " pegs at the correct place"
            putStrLn $ "==> " <> (show pos) <> " pegs with matching color but at the wrong place\n"
            guessLoop sec Human (n+1)
      
