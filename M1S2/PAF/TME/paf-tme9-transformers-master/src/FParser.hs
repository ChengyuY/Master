
-- | La parser de programmes ForthTrans
-- | en utilisant les parsers monadiques de MegaParsec

module FParser where

import Data.Char
import Data.Void

import Control.Monad

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

import FState

-- | le type du parser (à l'intérieur c'est un transformer !)
type FParser = Parsec Void String

-- >>> parseTest (char 'a' :: FParser Char) "a"
-- 'a'

-- >>> parseTest (char 'a' :: FParser Char) "b"
-- 1:1:
-- unexpected 'b'
-- expecting 'a'

-- >>> parseTest (string "ab" :: FParser String) "ab"
-- "ab"

-- >>> parseTest (string "ab" :: FParser String) "ac"
-- 1:1:
-- unexpected "ac"
-- expecting "ab"

-- >>> parseTest (string "ab" :: FParser String) "AB"
-- 1:1:
-- unexpected "AB"
-- expecting "ab"

-- >>> parseTest (string' "ab" :: FParser String) "AB"
-- "AB"

-- >>> parseTest (string' "ab" :: FParser String) "ab"
-- "ab"

-- >>> parseTest (letterChar :: FParser Char) "a"
-- 'a'

many1 :: FParser a -> FParser [a]
many1 p = (:) <$> p <*> (many p)

integer :: FParser FValue
integer = do
  sign <- try $ char '-'
          <|> return '+'
  num <- Lex.decimal
  return $ FInt $
    case sign of
    '-' -> (- num)
    _ -> num

     
-- >>> parseTest integer "42"
-- 42

-- >>> parseTest integer "-42"
-- 42

-- >>> parseTest integer "- 42"
-- 1:2:
-- unexpected space
-- expecting integer

-- >>> parseTest integer "+42"
-- 1:1:
-- unexpected '+'
-- expecting '-' or integer

double :: FParser FValue
double = do
  sign <- try $ char '-'
          <|> return '+'
  num <- Lex.float
  return $ FDouble $
    case sign of
    '-' -> (- num)
    _ -> num
  
-- >>> parseTest double "42.3"
-- 42.3

-- >>> parseTest double "42"
-- 1:3:
-- unexpected end of input
-- expecting '.', 'E', 'e', or digit

-- >>> parseTest double "-42"
-- 1:4:
-- unexpected end of input
-- expecting '.', 'E', 'e', or digit

-- >>> parseTest double "-42.3"
-- -42.3

-- >>> parseTest double "toto"
-- 1:1:
-- unexpected 't'
-- expecting '-' or digit

charLiteral :: FParser FValue
charLiteral = do
  char '\''
  ch <- Lex.charLiteral
  char '\''
  return $ FChar ch

-- >>> parseTest charLiteral "'a'"
-- 'a'

-- >>> parseTest charLiteral "'\''"
-- '\''

-- >>> parseTest charLiteral "42"
-- 1:1:
-- unexpected '4'
-- expecting '''


stringLiteral :: FParser FValue
stringLiteral = do
  str <- char '"' >> manyTill Lex.charLiteral (char '"')
  return $ FString str

-- >>> parseTest stringLiteral "\"Hello world\""
-- "Hello world"

-- >>> parseTest stringLiteral "\"Hello\\\" world\""
-- "Hello\" world"

-- >>> parseTest stringLiteral "Hello"
-- 1:1:
-- unexpected 'H'
-- expecting '"'

fvalue :: FParser FValue
fvalue =  try double
          <|> integer
          <|> charLiteral
          <|> stringLiteral

fval :: FParser FInstr
fval = do
  v <- fvalue
  return $ FVal v

-- >>> parseTest fval "42"
-- FVal 42

-- >>> parseTest fval "42.3"
-- FVal 42.3

-- >>> parseTest fval "'a'"
-- FVal 'a'

-- >>> parseTest fval "\"Hello world\""
-- FVal "Hello world"

-- >>> parseTest fval " "
-- 1:1:
-- unexpected space
-- expecting '"', ''', '-', digit, or integer

reservedChars :: [Char]
reservedChars = [':', ';', '"', '\'', ' ']

allDiff :: Eq a => [a] -> a -> Bool
allDiff xs x = all (\res -> res /= x) xs
 
firstLetter :: FParser Char
firstLetter = satisfy $ allDiff $ reservedChars <> ['0'..'9']

restLetter :: FParser Char
restLetter = satisfy $ allDiff $ reservedChars

word :: FParser String
word = do
  wordStr <- (:) <$> firstLetter <*> many restLetter
  return $ fmap toUpper wordStr

-- >>> parseTest word "Emit"  
-- "EMIT"

-- >>> parseTest word "Emit3"  
-- "EMIT3"

-- >>> parseTest word "3Emit"
-- 1:1:
-- unexpected '3'

-- >>> parseTest word "+"
-- "+"

-- >>> parseTest word ";"
-- 1:1:
-- unexpected ';'
  
colon :: FParser Char
colon = char ':'

semicolon :: FParser Char
semicolon = char ';'

finstr :: FParser FInstr
finstr = fval <|> (fmap FWord word)

fbody :: FParser FProgram
fbody = manyTill (do { space ; i <- finstr ; space ; return i }) semicolon

-- >>> parseTest fbody " PUSH 42 POP POP ;"
-- [FWord "PUSH",FVal 42,FWord "POP",FWord "POP"]

-- >>> parseTest fbody " PUSH POP + 42 POP;"
-- [FWord "PUSH",FWord "POP",FWord "+",FVal 42,FWord "POP"]

-- >>> parseTest fbody " PUSH POP -42 POP;"
-- [FWord "PUSH",FWord "POP",FVal -42,FWord "POP"]

-- >>> parseTest fbody ";"
-- []

fdefine :: FParser FProgram
fdefine = do
  space
  colon
  space
  w <- word
  space
  instrs <- fbody
  return $ [FDef w instrs]

-- >>> parseTest fdefine ": EMPTY ;"
-- [FDef "EMPTY" []]

-- >>> parseTest fdefine ": INCR 1 + ;"
-- [FDef "INCR" [FVal 1,FWord "+"]]

finstrs :: FParser FProgram
finstrs = many (do
                 instr <- finstr
                 space1
                 return instr)

fphrase :: FParser FProgram
fphrase = do
  prog <- try fdefine
          <|> fbody
  return prog

-- >>> parseTest fphrase ": TEN PUSH 10 ;"
-- [FDef "TEN" [FWord "PUSH",FVal 10]]

-- >>> parseTest fphrase "10 42 EMIT POP ;"
-- [FVal 10,FVal 42,FWord "EMIT",FWord "POP"]
