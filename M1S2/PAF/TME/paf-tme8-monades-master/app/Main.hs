module Main where

import Test.QuickCheck

import PAF_TME8_GenMonad

main :: IO ()
main = do
  putStrLn "Hello"
  x <- checkFrequency $ resize 100000 $ listOf genFreq
  putStrLn (show x)
  
