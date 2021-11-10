{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Data.Sequence as S ( fromList, Seq, empty, (<|))
import Data.Map.Strict as M ( fromList, Map, insert, empty, foldrWithKey, valid)
import Data.Text as T ( pack, Text )

data JSon = 
    JString Text 
    | JNumber Double 
    | JBool Bool 
    | JNull 
    | JArray (Seq JSon)
    | JObject (M.Map Text JSon)
    deriving (Show, Eq, Ord)

mkArray :: [JSon] -> JSon
mkArray = JArray . S.fromList 

mkObject :: [(T.Text, JSon)] -> JSon
mkObject = JObject . M.fromList

personSimon :: JSon
personSimon = mkObject
                [("nom", JString "Peyton Jones"),
                ("prenom", JString "Simon"),
                ("age", JNumber 62),
                ("travail", mkObject
                    [("role", JString "Principal Researcher"),
                    ("societe", JString "Microsoft Research")
                    ]
                ),
                ("contact", mkArray [
                    mkObject [("mel", JString "machin@truc.com")],
                    mkObject [("tel", JString "0612151515")]
                ]
                )
                ]


-- >>> personSimon
-- JObject (fromList [("age",JNumber 62.0),("contact",JArray (fromList [JObject (fromList [("mel",JString "machin@truc.com")]),JObject (fromList [("tel",JString "0612151515")])])),("nom",JString "Peyton Jones"),("prenom",JString "Simon"),("travail",JObject (fromList [("role",JString "Principal Researcher"),("societe",JString "Microsoft Research")]))])

class Encode a where
    toJson :: a -> JSon

infixr 5 ==>
(==>) :: Bool -> Bool -> Bool
(==>) x y = not x || y

-- Injectivite
lawEncodeInj :: (Encode a, Eq a) => a -> a -> Bool
lawEncodeInj x y = (x /= y) ==> (toJson x /= toJson y)

instance Encode Double where
    toJson :: Double -> JSon
    toJson = JNumber

instance Encode Int where
    toJson :: Int -> JSon
    toJson = JNumber . fromIntegral

-- >>> toJson (42 :: Int)
-- JNumber 42.0
-- >>> lawEncodeInj (4.2 :: Double) 3.0
-- True
-- >>> lawEncodeInj (42 :: Int) 3
-- True

instance Encode () where
    toJson :: () -> JSon
    toJson _ = JNull

instance Encode Bool where
    toJson :: Bool -> JSon
    toJson = JBool

instance Encode Text where
    toJson :: Text -> JSon
    toJson = JString

class ShowText a where
    showText :: a -> Text

instance ShowText Text where
    showText :: Text -> Text
    showText = id

instance ShowText String where
    showText :: String -> Text
    showText = T.pack

instance Encode a => Encode (Seq a) where
    toJson :: Seq a -> JSon
    toJson = JArray . fmap toJson

instance (ShowText k, Encode a) => Encode (Map k a) where
    toJson :: Map k a -> JSon
    toJson = JObject . M.foldrWithKey aux M.empty
        where aux clef val = M.insert (showText clef) (toJson val)

instance (ShowText a, Encode b) => Encode [(a,b)] where
    toJson :: [(a,b)] -> JSon
    toJson = mkObject . map (\(x,y) -> (showText x , toJson y))

data Person = Person {
    name :: Text
    , firstName :: Text
    , age :: Int
    , work :: Work
    , contacts :: Seq Contact
    } deriving (Show, Eq)

data Work = Work { company :: Text, position :: Text}
    deriving (Show, Eq)
data Contact = Contact { conType :: ContactType, contInfo :: Text }
    deriving (Show, Eq)
data ContactType = Phone | Email
    deriving (Show, Eq)

instance Encode Work where
    toJson :: Work -> JSon
    toJson (Work comp pos) = toJson [("societe" :: Text, comp),
                                ("position", pos)]

instance Encode Contact where
    toJson :: Contact -> JSon
    toJson (Contact ctyp info) = 
        let (conType :: Text) = case ctyp of  
                Phone -> "tel"
                Email -> "mel"
        in toJson [(conType, info)]

-- ??? Personne --

class Decode a where
    fromJson :: JSon -> Maybe a

decodeBool :: JSon -> Maybe Bool
decodeBool (JBool x) = Just x
decodeBool _ = Nothing

decodeNull :: JSon -> Maybe ()
decodeNull JNull = Just ()
decodeNull _ = Nothing

decodeNumber :: JSon -> Maybe Double
decodeNumber (JNumber x) = Just x
decodeNumber _ = Nothing

decodeString :: JSon -> Maybe Text
decodeString (JString t) = Just t
decodeString _ = Nothing

instance Decode Bool where
    fromJson = decodeBool

instance Decode () where
    fromJson = decodeNull

instance Decode Double where
    fromJson = decodeNumber

instance Decode Text where
    fromJson = decodeString


decodeSeq :: (JSon -> Maybe a) -> JSon -> Maybe (Seq a)
decodeSeq conv (JArray xs) = foldr enDecodeUn (Just S.empty) xs
    where enDecodeUn x (Just res) = case conv x of
                        Just v -> Just (v S.<| res)
                        Nothing -> Nothing 
          enDecodeUn _ Nothing = Nothing
decodeSeq _ _ = Nothing

instance (Decode a) => Decode (Seq a) where
    fromJson :: JSon -> Maybe (Seq a)
    fromJson = decodeSeq fromJson 


decodeClefValeur :: (Json -> Maybe a) -> Text -> JSon -> Maybe a
decodeClefValeur conv clef (JObject m) = 
    case M.lookup clef m of
        Nothing -> Nothing 
        Just val -> conv val
decodeClefValeur _ _ _ = Nothing   

decodeContact :: JSon -> Maybe Contact
decodeContact js =
    case decodeClefValeur decodeString "tel" js of
        Just v -> Just $ Contact Phone v
        Nothing -> case decodeClefValeur decodeString "mel" js of
            Just v -> Just $ Contact Email v
            Nothing -> Nothing

instance Decode Contact where
    fromJson = decodeContact

decodeWork :: JSon -> Maybe Work
decodeWork js = case decodeClefValeur decodeString "societe" js of
    Nothing -> Nothing 
    Just s -> case decodeClefValeur decodeString "position" js of
        Nothing -> Nothing 
        Just p -> Just $ Work s p

instance Decode Work where
    fromJson = decodeWork



loiDecodeBij :: (Eq a, Decode a, Encode a) => a -> Bool
loiDecodeBij x = case fromJson (toJson x) of
    Nothing -> error "encodage"
    Just y -> y == x

loiEncodeBij :: (Eq a, Decode a, Encode a) => JSon -> Bool
loiEncodeBij x = case fromJson x of
    Nothing -> error "encodage"
    Just y -> toJson y == x

