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

newtype CommunalDTO = CommunalDTO
  { communalCapacity :: Integer
  } deriving (Eq, Show, Generic)

communalJSONOptions :: Options
communalJSONOptions =
  defaultOptions {
    fieldLabelModifier = \s -> case s of
      "communalCapacity" -> "capacity"
      _ -> s }

instance FromJSON CommunalDTO where
  parseJSON = genericParseJSON communalJSONOptions
instance ToJSON CommunalDTO where
  toJSON = genericToJSON communalJSONOptions
  toEncoding = genericToEncoding communalJSONOptions

data SingleDTO = SingleDTO
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

instance FromJSON SingleDTO where
  parseJSON = genericParseJSON singleJSONOptions
instance ToJSON SingleDTO where
  toJSON = genericToJSON singleJSONOptions
  toEncoding = genericToEncoding singleJSONOptions

data TableDTO = TableDTO
  { singleTable :: Maybe SingleDTO
  , communalTable :: Maybe CommunalDTO
  } deriving (Eq, Show, Generic)

tableJSONOptions :: Options
tableJSONOptions =
  defaultOptions { omitNothingFields = True }

instance FromJSON TableDTO where
  parseJSON = genericParseJSON tableJSONOptions
instance ToJSON TableDTO where
  toJSON = genericToJSON tableJSONOptions
  toEncoding = genericToEncoding tableJSONOptions

tryParseTable :: TableDTO -> Maybe Table
tryParseTable (TableDTO (Just (SingleDTO c m)) Nothing) = trySingleTable c m
tryParseTable (TableDTO Nothing (Just (CommunalDTO c))) = tryCommunalTable c
tryParseTable _ = Nothing

toTableDTO :: Table -> TableDTO
toTableDTO (Single (SingleT (N c) (N m))) =
  TableDTO (Just (SingleDTO c m)) Nothing
toTableDTO (Communal (N c)) = TableDTO Nothing (Just (CommunalDTO c))