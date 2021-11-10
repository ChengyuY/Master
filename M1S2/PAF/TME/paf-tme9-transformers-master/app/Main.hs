
{-# LANGUAGE BangPatterns #-}

module Main where

import System.Console.Haskeline

import Text.Megaparsec

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict -- Forth est strict

import FState
import FInterp

import FParser

execProgram :: Bool -> FProgram -> FMachine -> IO FMachine
execProgram dbg prog fm = do
  f <- pure $ runStateT $ runExceptT $ execProg dbg prog
  (!v, fm') <- f fm
  when dbg $ putStrLn (show fm')
  return fm'

data DebugMode = DEBUG | NODEBUG

debugFlag :: DebugMode -> Bool
debugFlag DEBUG = True
debugFlag NODEBUG = False

forthRepl :: FMachine -> DebugMode -> InputT IO FMachine
forthRepl fm dbg = do
  minput <- getInputLine "> "
  case minput of
    Nothing -> return fm
    Just "!debug" -> do { liftIO $ putStrLn "Debug mode ON." ; forthRepl fm DEBUG }
    Just "!nodebug" -> do { liftIO $ putStrLn "Debug mode OFF." ; forthRepl fm NODEBUG }
    Just "!reset" -> do { liftIO $ putStrLn "Resetting FMachine." ; forthRepl initialFMachine dbg }
    Just "!stack" -> do { liftIO $ putStrLn $ "Machine stack: " <> (show (fStack fm)) ; forthRepl fm dbg }
    Just "!q" -> do { liftIO $ putStrLn "Bye bye." ; return fm }
    Just "!quit" -> do { liftIO $ putStrLn "Bye bye." ; return fm }
    Just input -> case runParser fphrase "<console>" input of
                    Right prog -> do fm' <- liftIO $ execProgram (debugFlag dbg) prog fm
                                     forthRepl fm' dbg 
                    Left err -> do liftIO $ putStrLn (errorBundlePretty err)
                                   forthRepl fm dbg

banner :: IO ()
banner = do
  putStrLn "ForthTrans Interpreter v0.2 -- Copyright (C) 2020 PAF"
  putStrLn "----"
  putStrLn "Type !stack for showing current forth stack"
  putStrLn "Type !debug for debug mode / !nodebug to reverse"
  putStrLn "Type !reset for resetting forth machine"
  putStrLn "Type !q (or !quit) for quitting"
  putStrLn ""

main :: IO FMachine
main = do
  banner
  runInputT defaultSettings (forthRepl initialFMachine NODEBUG)
