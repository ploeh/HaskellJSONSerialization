{-# LANGUAGE OverloadedStrings #-}
module JSONSerialization where

import Data.ByteString.Lazy (ByteString)
import Restaurants
import Data.Aeson

newtype JSONTable = JSONTable Table deriving (Eq, Show)

instance ToJSON JSONTable where
  toJSON (JSONTable (Single (SingleT (N c) (N m)))) =
    object ["singleTable" .= object [
      "capacity" .= c,
      "minimalReservation" .= m]]
  toJSON (JSONTable (Communal (N c))) =
    object ["communalTable" .= object ["capacity" .= c]]

serializeTable :: Table -> ByteString
serializeTable = encode . JSONTable