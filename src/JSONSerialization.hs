{-# LANGUAGE OverloadedStrings #-}
module JSONSerialization where

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