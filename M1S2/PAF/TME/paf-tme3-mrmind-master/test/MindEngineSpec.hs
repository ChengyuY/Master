
module MindEngineSpec where

import qualified Data.Set as Set

import qualified Data.Sequence as Seq

import MindEngine

import Test.Hspec

-- Example from: https://www.wikihow.com/Play-Mastermind
exSecret = mkSecret $ Seq.fromList [Yellow, Yellow, Green, Blue ]

-- check individual Pegs

initFeedbackSpec =
  describe "initFeedback" $ do

  it "returns all secret pegs unmarked" $ do
    initFeedback exSecret
      `shouldBe` (Seq.fromList [(Yellow, Unmarked), (Yellow, Unmarked), (Green, Unmarked), (Blue, Unmarked)])

markCorrectOneSpec =
  describe "markCorrectOne" $ do

  it "returns a mark 'correct' if the guess and secret pegs are equal" $ do
    markCorrectOne Yellow (Yellow, Unmarked) `shouldBe` (PEmpty, (Yellow, MarkedCorrect))

  it "returns an unchanged mark if the guess and secret pegs are inequal" $ do
    markCorrectOne Blue (Yellow, Unmarked) `shouldBe` (Blue, (Yellow, Unmarked))

markCorrectSpec =
  describe "markCorrect" $ do

  it "returns no correct mark if the whole guess does not correspond to the secret" $ do
    let guess = Seq.fromList [Blue, Blue, Blue, Red]
        feedback = initFeedback exSecret
        res = markCorrect guess feedback
      in res `shouldBe` (guess, feedback)

  it "returns a single correct mark and empty the corresponding guess" $ do
    let guess = Seq.fromList [Blue, Blue, Green, Red]
        feedback = initFeedback exSecret
        res = markCorrect guess feedback
      in res `shouldBe` (Seq.fromList [Blue, Blue, PEmpty, Red]
                        ,Seq.fromList [(Yellow, Unmarked), (Yellow, Unmarked), (Green, MarkedCorrect), (Blue, Unmarked)])

  it "returns a two correct marks and empty the corresponding guesses" $ do
    let guess = Seq.fromList [Yellow, Blue, Green, Red]
        feedback = initFeedback exSecret
        res = markCorrect guess feedback
      in res `shouldBe` (Seq.fromList [PEmpty, Blue, PEmpty, Red]
                        ,Seq.fromList [(Yellow, MarkedCorrect), (Yellow, Unmarked), (Green, MarkedCorrect), (Blue, Unmarked)])

  it "returns all correct marks and empty all guesses" $ do
    let guess = Seq.fromList [Yellow, Yellow, Green, Blue]
        feedback = initFeedback exSecret
        res = markCorrect guess feedback
      in res `shouldBe` (Seq.fromList [PEmpty, PEmpty, PEmpty, PEmpty]
                        ,Seq.fromList [(Yellow, MarkedCorrect), (Yellow, MarkedCorrect), (Green, MarkedCorrect), (Blue, MarkedCorrect)])


markPositionSpec =
  describe "markPosition" $ do

  it "returns a single position mark for first guess" $ do
    markPosition (Seq.fromList [Blue, Red, Red, Red])
                 (Seq.fromList [(Yellow, Unmarked), (Yellow, Unmarked), (Green, Unmarked), (Blue, Unmarked)])
      `shouldBe` (Seq.fromList [(Yellow, Unmarked), (Yellow, Unmarked), (Green, Unmarked), (Blue, MarkedPosition)])

  it "does not mark the position if it is already correct" $ do
    markPosition (Seq.fromList [Blue, Red, Red, Blue])
                 (Seq.fromList [(Yellow, Unmarked), (Yellow, Unmarked), (Green, Unmarked), (Blue, MarkedCorrect)])
      `shouldBe` (Seq.fromList [(Yellow, Unmarked), (Yellow, Unmarked), (Green, Unmarked), (Blue, MarkedCorrect)])

  it "skips empty guesses and returns a single position mark for third guess" $ do
    markPosition (Seq.fromList [PEmpty, PEmpty, Blue, Red])
                 (Seq.fromList [(Yellow, Unmarked), (Yellow, Unmarked), (Green, Unmarked), (Blue, Unmarked)])
      `shouldBe` (Seq.fromList [(Yellow, Unmarked), (Yellow, Unmarked), (Green, Unmarked), (Blue, MarkedPosition)])

  it "favors the leftmost match" $ do
    markPosition (Seq.fromList [PEmpty, Red, Red, Yellow])
                 (Seq.fromList [(Yellow, Unmarked), (Yellow, Unmarked), (Green, Unmarked), (Blue, Unmarked)])
      `shouldBe` (Seq.fromList [(Yellow, MarkedPosition), (Yellow, Unmarked), (Green, Unmarked), (Blue, Unmarked)])

  it "matches multiple times if needed" $ do
    markPosition (Seq.fromList [PEmpty, Red, Yellow, Yellow])
                 (Seq.fromList [(Yellow, Unmarked), (Yellow, Unmarked), (Green, Unmarked), (Blue, Unmarked)])
      `shouldBe` (Seq.fromList [(Yellow, MarkedPosition), (Yellow, MarkedPosition), (Green, Unmarked), (Blue, Unmarked)])

  it "it also works with multiple matches" $ do
    markPosition (Seq.fromList [PEmpty, Green, Blue, Yellow])
                 (Seq.fromList [(Yellow, Unmarked), (Yellow, Unmarked), (Green, Unmarked), (Blue, Unmarked)])
      `shouldBe` (Seq.fromList [(Yellow, MarkedPosition), (Yellow, Unmarked), (Green, MarkedPosition), (Blue, MarkedPosition)])

-- tests à compléter
verifySpec = do
  describe "verify" $ do
    it "correctly answers with 0 correct peg and 0 position" $ do
     verify exSecret (Seq.fromList [White, White,White, White]) `shouldBe` (Answer 0 0)
    it "correctly answers with 1 correct peg and 0 position" $ do
      verify exSecret (Seq.fromList [Yellow, White,White, White]) `shouldBe` (Answer 1 0)
    it "correctly answers with 1 correct peg and 1 position" $ do
      verify exSecret (Seq.fromList [Yellow , Green ,White, White]) `shouldBe` (Answer 1 1)
    it "correctly answers with 2 correct peg and 1 position" $ do
     verify exSecret (Seq.fromList [Yellow , White  ,Green ,Yellow ]) `shouldBe` (Answer 2 1)
    it "correctly answers with 2 correct peg and 2 position" $ do
      verify exSecret (Seq.fromList [Yellow , Blue ,Green , Yellow ]) `shouldBe` (Answer 2 2)
    it "correctly answers with 0 correct peg and 4 positions" $ do
      verify exSecret (Seq.fromList [Green  , Blue ,Yellow  , Yellow ]) `shouldBe` (Answer 0 4)
    it "correctly answers with 4 correct peg and 0 positions" $ do
     verify exSecret (Seq.fromList [Yellow , Yellow  ,Green , Blue  ]) `shouldBe` (Answer 4 0)


winningSpec =
  describe "winning" $ do
  it "says that there's a win in the correct situation" $ do
    winning exSecret  (verify exSecret (Seq.fromList [Yellow, Yellow, Green, Blue])) `shouldBe` True
  it "says False otherwise" $ do
    winning exSecret  (verify exSecret (Seq.fromList [Yellow, Yellow, Green, Red])) `shouldBe` False


engineSpec = do
  initFeedbackSpec
  markCorrectOneSpec
  markCorrectSpec
  markPositionSpec
  verifySpec
  winningSpec






