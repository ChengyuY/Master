
module MindEngine where

import Data.Foldable

-- utilitaires de séquences
import Data.Sequence (Seq, Seq (..))
import qualified Data.Sequence as Seq

-- utilitaires d'ensembles
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Foldable as F

import Debug.Trace

-- Note: 8 colors because it's the standard ANSI colors
data Peg =
  PEmpty
  | Black
  | Blue
  | Green
  | Yellow
  | Cyan
  | White
  | Magenta
  | Red
  deriving (Show, Eq, Ord)

data FeedbackMark =
  MarkedCorrect
  | MarkedPosition
  | Unmarked
  deriving (Show, Eq)

data Secret = Secret { pegs :: (Seq Peg)
                     , size :: Int }
            deriving (Show, Eq)


-- smart constructor for secrets
mkSecret :: Seq Peg -> Secret
mkSecret pegs = Secret pegs (length pegs)
                
type Guess = Seq Peg
type Feedback = Seq (Peg, FeedbackMark)

data Answer = Answer { correct :: Int, position :: Int }
  deriving (Show, Eq)

-- runtime error if not a good guess
safeGuess :: Secret -> Guess -> Guess
safeGuess secret guess =
  if (size secret) /= (length guess)
  then error "Wrong guess size (please report)"
  else guess

wrongGuess :: Secret -> Guess -> Bool
wrongGuess secret guess = (size secret) /= length guess

initFeedback :: Secret -> Feedback
initFeedback (Secret sec _) =
  fmap (\p -> (p, Unmarked)) sec 

markCorrectOne :: Peg -> (Peg, FeedbackMark) -> (Peg, (Peg, FeedbackMark))
markCorrectOne gpeg (speg, mk) | gpeg == speg = (PEmpty, (speg, MarkedCorrect))
                               | otherwise = (gpeg, (speg, mk))
                     
markCorrect :: Guess -> Feedback -> (Guess, Feedback)
markCorrect gus fed = (Seq.fromList (zipWith (\g fd -> fst (markCorrectOne g fd)) (F.toList gus) (F.toList fed)),
                       Seq.fromList (zipWith (\g fd -> snd (markCorrectOne g fd)) (F.toList gus) (F.toList fed)))

-- >>> markCorrect (Seq.fromList [Yellow, Blue, Green, Red]) (Seq.fromList [(Yellow, Unmarked), (Yellow, Unmarked), (Green, Unmarked), (Blue, Unmarked)])
-- >>> 
-- (fromList [PEmpty,Blue,PEmpty,Red],fromList [(Yellow,MarkedCorrect),(Yellow,Unmarked),(Green,MarkedCorrect),(Blue,Unmarked)])

checkPos :: Guess -> (Peg, FeedbackMark) -> Int -> Int -> ((Peg, FeedbackMark),Guess)
checkPos gus (speg, mk) i pos 
  | mk == MarkedCorrect || mk == MarkedPosition = ((speg, mk),gus)
  | i == 3 =
        if Seq.index gus i == speg then if i == pos 
          then ((speg, MarkedCorrect), Seq.update i PEmpty gus)
          else ((speg, MarkedPosition), Seq.update i PEmpty gus)
      else
        ((speg,mk),gus)
  | Seq.index gus i == speg =
          if i == pos 
            then ((speg, MarkedCorrect), Seq.update i PEmpty gus)
            else ((speg, MarkedPosition), Seq.update i PEmpty gus)
  | otherwise = checkPos gus (speg, mk) (i+1) pos

-- >>> snd (checkPos (Seq.fromList [PEmpty,Red,Yellow,Yellow]) (Yellow, Unmarked) 0 0)  
-- fromList [PEmpty,Red,PEmpty,Yellow]

markPosition :: Guess -> Feedback -> Feedback
markPosition gus fed = mark gus gus fed 0
  where
    mark :: Guess -> Guess -> Feedback -> Int -> Feedback
    mark before after fed i 
      | i == 0 = fst(checkPos before (Seq.index fed i) 0 i) Seq.<| mark before (snd(checkPos before (Seq.index fed i) 0 i)) fed (i+1)
      | i == 3 = Seq.fromList [fst(checkPos after (Seq.index fed i) 0 i)]
      | otherwise = fst(checkPos after (Seq.index fed i) 0 i) Seq.<| mark before (snd(checkPos after (Seq.index fed i) 0 i)) fed (i+1)

-- >>> markPosition (Seq.fromList [PEmpty, Red, Yellow, Yellow]) (Seq.fromList [(Yellow, Unmarked), (Yellow, Unmarked), (Green, Unmarked), (Blue, Unmarked)])
-- fromList [(Yellow,MarkedPosition),(Yellow,MarkedPosition),(Green,Unmarked),(Blue,Unmarked)]

-- >>> markPosition (Seq.fromList [Blue, Red, Red, Red]) (Seq.fromList [(Yellow, Unmarked), (Yellow, Unmarked), (Green, Unmarked), (Blue, Unmarked)])
-- fromList [(Yellow,Unmarked),(Yellow,Unmarked),(Green,Unmarked),(Blue,MarkedPosition)]

-- >>> markPosition (Seq.fromList [Blue, Red, Red, Blue]) (Seq.fromList [(Yellow, Unmarked), (Yellow, Unmarked), (Green, Unmarked), (Blue, MarkedCorrect)])
-- fromList [(Yellow,Unmarked),(Yellow,Unmarked),(Green,Unmarked),(Blue,MarkedCorrect)]

-- >>> markPosition (Seq.fromList [PEmpty, PEmpty, Blue, Red]) (Seq.fromList [(Yellow, Unmarked), (Yellow, Unmarked), (Green, Unmarked), (Blue, Unmarked)])
-- fromList [(Yellow,Unmarked),(Yellow,Unmarked),(Green,Unmarked),(Blue,MarkedPosition)]

-- >>> markPosition (Seq.fromList [PEmpty, Red, Red, Yellow]) (Seq.fromList [(Yellow, Unmarked), (Yellow, Unmarked), (Green, Unmarked), (Blue, Unmarked)])
-- fromList [(Yellow,MarkedPosition),(Yellow,Unmarked),(Green,Unmarked),(Blue,Unmarked)]

-- >>> markPosition (Seq.fromList [PEmpty, Red, Yellow, Yellow]) (Seq.fromList [(Yellow, Unmarked), (Yellow, Unmarked), (Green, Unmarked), (Blue, Unmarked)])
-- fromList [(Yellow,MarkedPosition),(Yellow,MarkedPosition),(Green,Unmarked),(Blue,Unmarked)]

-- >>> markPosition (Seq.fromList [PEmpty, Green, Blue, Yellow]) (Seq.fromList [(Yellow, Unmarked), (Yellow, Unmarked), (Green, Unmarked), (Blue, Unmarked)])
-- fromList [(Yellow,MarkedPosition),(Yellow,Unmarked),(Green,MarkedPosition),(Blue,MarkedPosition)]

verify :: Secret -> Guess -> Answer
verify secret guess = 
  let (guess', fb) = markCorrect (safeGuess secret guess) (initFeedback secret)
      fb' = markPosition guess' fb
  in foldr verifyAux (Answer 0 0) (fmap snd fb')
  where verifyAux :: FeedbackMark -> Answer -> Answer
        verifyAux MarkedCorrect (Answer cor pos)  = Answer (cor + 1) pos 
        verifyAux MarkedPosition (Answer cor pos)  = Answer cor (pos + 1)
        verifyAux _ ans = ans

winning :: Secret -> Answer -> Bool
winning (Secret _ size) (Answer cor _) = size == cor 
