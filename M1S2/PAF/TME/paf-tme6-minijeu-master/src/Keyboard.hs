
module Keyboard where

import SDL

import Data.List (foldl')

import Data.Set (Set)
import qualified Data.Set as S

type Keyboard = Set Keycode

type Mouse = Maybe (V2 Int)

-- | création de la structure d'état de clavier (vide)
createKeyboard :: Keyboard
createKeyboard = S.empty

handleEventKeyboard :: Event -> Keyboard -> Keyboard
handleEventKeyboard event kbd =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      if keyboardEventKeyMotion keyboardEvent == Pressed
      then S.insert (keysymKeycode (keyboardEventKeysym keyboardEvent)) kbd
      else if keyboardEventKeyMotion keyboardEvent == Released
           then S.delete (keysymKeycode (keyboardEventKeysym keyboardEvent)) kbd
           else kbd
    _ -> kbd

handleEventMouse :: Event  -> Mouse -> Mouse
handleEventMouse event mouse = 
  case eventPayload event of
    MouseButtonEvent mouseButton ->
            let P coordinates = fmap fromIntegral (mouseButtonEventPos mouseButton) in
                Just coordinates
    _ -> Nothing

handleEvent :: Event -> (Keyboard, Mouse) -> (Keyboard, Mouse)
handleEvent event (kbd,ms) =
  (handleEventKeyboard event kbd,handleEventMouse event ms)

-- | prise en compte des événements SDL2 pour mettre à jour l'état du clavier
--handleEvents :: [Event] -> Keyboard -> Keyboard
--handleEvents events kbd = foldl' (flip handleEvent) kbd events

handleEvents :: [Event] -> Keyboard ->  (Keyboard, Mouse)
handleEvents events kbd = foldl' (flip handleEvent) (kbd, Nothing) events

-- | quelques noms de *keycode*
keycodeName :: Keycode -> Char
keycodeName KeycodeA = 'a'
keycodeName KeycodeB = 'b'
keycodeName KeycodeC = 'c'
keycodeName KeycodeD = 'd'
keycodeName KeycodeE = 'e'
keycodeName KeycodeF = 'f'
keycodeName KeycodeG = 'g'
keycodeName KeycodeH = 'h'
keycodeName KeycodeI = 'i'
keycodeName KeycodeJ = 'j'
keycodeName KeycodeK = 'k'
keycodeName KeycodeL = 'l'
keycodeName KeycodeM = 'm'
keycodeName KeycodeN = 'n'
keycodeName KeycodeO = 'o'
keycodeName KeycodeP = 'p'
keycodeName KeycodeQ = 'q'
keycodeName KeycodeR = 'r'
keycodeName KeycodeS = 's'
keycodeName KeycodeT = 't'
keycodeName KeycodeU = 'u'
keycodeName KeycodeV = 'v'
keycodeName KeycodeW = 'w'
keycodeName KeycodeX = 'x'
keycodeName KeycodeY = 'y'
keycodeName KeycodeZ = 'z'
keycodeName _ = '-'

-- | Vérifies sir le *keycode* spécificé est actuellement
-- | actif sur le clavier.
keypressed :: Keycode -> Keyboard -> Bool
keypressed kc kbd = S.member kc kbd
