{-# LANGUAGE OverloadedStrings #-}
module Main where

import Restaurants
import JSONSerialization
import Test.HUnit
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.Framework (defaultMain)

main :: IO ()
main = defaultMain $ hUnitTestToTests $ TestList [
  "Serialize communal table for 42" ~:
    let table = tryCommunalTable 42
        actual = serializeTable <$> table
    in Just "{\"communalTable\":{\"capacity\":42}}" ~=? actual
  ,
  "Serialize single table for 2" ~:
    let table = trySingleTable 2 1
        actual = serializeTable <$> table
    in Just "{\"singleTable\":{\"capacity\":2,\"minimalReservation\":1}}" ~=? actual
  ,
  "Deserialize communal table for 42" ~:
    let json = "{\"communalTable\":{\"capacity\":42}}"
        actual = tryDeserializeTable json
    in tryCommunalTable 42 ~=? actual
  ,
  "Deserialize single table for 4" ~:
    let json = "{\"singleTable\":{\"capacity\":4,\"minimalReservation\":3}}"
        actual = tryDeserializeTable json
    in trySingleTable 4 3 ~=? actual
  ]