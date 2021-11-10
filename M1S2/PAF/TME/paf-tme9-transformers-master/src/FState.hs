
-- | Représentation de l'état de la machine Forth
module FState where

import Data.Functor.Identity (Identity (..))

import Data.Map (Map)
import qualified Data.Map as M

import Data.Sequence (Seq (..))
import qualified Data.Sequence as S

import Control.Monad.Trans.State.Strict


-- | Utilitaires

-- | plus court à écrire
mkSeq :: [a] -> Seq a
mkSeq = S.fromList

-- | Les types de valeurs
data FValue =
  FNone             -- pas de valeur  
  | FBool Bool      -- booléens (pas en Forth standard, mais c'est mieux comme ça)
  | FInt Int        -- entiers machine
  | FDouble Double  -- flottants
  | FChar Char      -- caractères
  | FString String  -- chaînes
  | FNext FInstr    -- prochaine instruction (valeur spéciale pour fnext)
  deriving (Eq)

instance Show FValue where
  show FNone = "."
  show (FBool b) = show b
  show (FInt n) = show n
  show (FDouble x) = show x
  show (FChar c) = show c
  show (FString s) = show s
  show (FNext instr) = "<" <> (show instr) <> ">"

-- | Les instructions élémentaires
data FInstr =
  FVal FValue         -- une valeur comme instruction
  | FPrim FPrimitive  -- une primitive
  | FWord String      -- un mot
  | FDef String FProgram -- une définition de mot
  deriving (Show, Eq)

-- | Les primitives
data FPrimitive =
  POP
  | EMIT
  deriving (Show, Eq)

-- | Les erreurs
data FError =
  FErrStackEmpty
  | FErrNotImplemented String
  | FErrNoSuchWord String
  | FErrEmptyProgram
  deriving (Eq)

instance Show FError where
  show FErrStackEmpty = "Empty Stack"
  show (FErrNotImplemented s) = "Not implemented: " <> s
  show (FErrNoSuchWord w) = "No such word: " <> w
  show FErrEmptyProgram = "End of prorgram"

type FProgram = [FInstr]

-- | L'état de la machine Forth
data FMachine = 
  FMachine { fStack :: [FValue]
           , fWords :: Map String (Seq FProgram)
           , fProg :: FProgram
           }
  deriving (Eq)

instance Show FMachine where
  show (FMachine { fStack = stk, fProg = p }) =
    "<stack=" <> (show stk) <> ", prog=" <> (show p) <> ">"

initialFMachine :: FMachine
initialFMachine = FMachine {
  fStack = []
  , fWords = M.fromList [("POP", mkSeq [[FPrim POP]])
                        ,("EMIT", mkSeq [[FPrim EMIT]])
                        ]
  , fProg = []
  }


endOfProgram :: FMachine -> Bool
endOfProgram (FMachine { fProg = [] }) = True
endOfProgram _ = False

type ForthResult = Either FError FValue

fnext :: FMachine -> (ForthResult, FMachine)
fnext fm@(FMachine { fProg = [] }) = (Left FErrEmptyProgram, fm)
fnext fm@(FMachine { fProg = (x:xs) }) = (Right $ FNext x, fm { fProg = xs })

-- | dépiler
fpop :: FMachine -> (ForthResult, FMachine)
fpop fm@(FMachine { fStack = [] }) = (Left FErrStackEmpty, fm)
fpop fm@(FMachine { fStack = (x:xs) }) = (Right x, fm { fStack = xs })

-- | empiler
fpush :: FValue -> FMachine -> (ForthResult, FMachine)
fpush x fm@(FMachine { fStack = xs }) = (Right FNone, fm { fStack = (x:xs) })

-- | chercher et utiliser la (dernière) définition d'un mot
fword :: String -> FMachine -> (ForthResult, FMachine)
fword word fm@(FMachine { fWords = fws, fProg = prog }) =
  case M.lookup word fws of
    Nothing -> (Left $ FErrNoSuchWord word, fm)
    Just Empty -> error "Empty word sequence"
    Just (p :<| _) -> (Right FNone, fm { fProg = p <> prog})

-- | charger un (nouveau) programme dans la FMachine
fload :: FProgram -> FMachine -> (ForthResult, FMachine)
fload prog fm = (Right FNone, fm { fProg = prog })

-- | enregistrer une (nouvelle) définition pour un mot
fdef :: String -> FProgram -> FMachine -> (ForthResult, FMachine)
fdef word prog fm@(FMachine { fWords = fws }) =
  (Right FNone, fm { fWords = case M.lookup word fws of
                                Nothing -> M.insert word (S.singleton prog) fws 
                                Just progs -> M.insert word (prog :<| progs) fws })


