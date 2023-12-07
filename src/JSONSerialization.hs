{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module JSONSerialization where

import GHC.Generics
import Data.ByteString.Lazy (ByteString)
import Restaurants
import Data.Aeson

newtype JSONTable = JSONTable Table deriving (Eq, Show)

instance FromJSON JSONTable where
  parseJSON (Object v) = do
    single <- v .:? "singleTable"
    communal <- v .:? "communalTable"
    case (single, communal) of
      (Just s, Nothing) -> do
        capacity <- s .: "capacity"
        minimal <- s .: "minimalReservation"
        case trySingleTable capacity minimal of
          Nothing -> fail "Expected natural numbers."
          Just t -> return $ JSONTable t
      (Nothing, Just c) -> do
        capacity <- c .: "capacity"
        case tryCommunalTable capacity of
          Nothing -> fail "Expected a natural number."
          Just t -> return $ JSONTable t
      _ -> fail "Expected exactly one of singleTable or communalTable."
  parseJSON _ = fail "Expected an object."

instance ToJSON JSONTable where
  toJSON (JSONTable (Single (SingleT (N c) (N m)))) =
    object ["singleTable" .= object [
      "capacity" .= c,
      "minimalReservation" .= m]]
  toJSON (JSONTable (Communal (N c))) =
    object ["communalTable" .= object ["capacity" .= c]]

serializeTable :: Table -> ByteString
serializeTable = encode . JSONTable

tryDeserializeTable :: ByteString -> Maybe Table
tryDeserializeTable = fmap (\(JSONTable t) -> t) . decode

-- Generics-based JSON (de)serialization

newtype CommunalDTR = CommunalDTR
  { communalCapacity :: Integer
  } deriving (Eq, Show, Generic)

communalJSONOptions :: Options
communalJSONOptions =
  defaultOptions {
    fieldLabelModifier = \s -> case s of
      "communalCapacity" -> "capacity"
      _ -> s }

instance FromJSON CommunalDTR where
  parseJSON = genericParseJSON communalJSONOptions
instance ToJSON CommunalDTR where
  toJSON = genericToJSON communalJSONOptions
  toEncoding = genericToEncoding communalJSONOptions

data SingleDTR = SingleDTR
  { singleCapacity :: Integer
  , minimalReservation :: Integer
  } deriving (Eq, Show, Generic)

singleJSONOptions :: Options
singleJSONOptions =
  defaultOptions {
    fieldLabelModifier = \s -> case s of
      "singleCapacity" -> "capacity"
      "minimalReservation" -> "minimalReservation"
      _ -> s }

instance FromJSON SingleDTR where
  parseJSON = genericParseJSON singleJSONOptions
instance ToJSON SingleDTR where
  toJSON = genericToJSON singleJSONOptions
  toEncoding = genericToEncoding singleJSONOptions

data TableTDR = TableTDR
  { singleTable :: Maybe SingleDTR
  , communalTable :: Maybe CommunalDTR
  } deriving (Eq, Show, Generic)

tableJSONOptions :: Options
tableJSONOptions =
  defaultOptions { omitNothingFields = True }

instance FromJSON TableTDR where
  parseJSON = genericParseJSON tableJSONOptions
instance ToJSON TableTDR where
  toJSON = genericToJSON tableJSONOptions
  toEncoding = genericToEncoding tableJSONOptions

tryParseTable :: TableTDR -> Maybe Table
tryParseTable (TableTDR (Just (SingleDTR c m)) Nothing) =
  trySingleTable c m
tryParseTable (TableTDR Nothing (Just (CommunalDTR c))) =
  tryCommunalTable c
tryParseTable _ = Nothing

toTableDTR :: Table -> TableTDR
toTableDTR (Single (SingleT (N c) (N m))) =
  TableTDR (Just (SingleDTR c m)) Nothing
toTableDTR (Communal (N c)) = TableTDR Nothing (Just (CommunalDTR c))