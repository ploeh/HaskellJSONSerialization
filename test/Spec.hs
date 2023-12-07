{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Data.Aeson (decode, encode)
import Text.RawString.QQ(r)
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
    in Just [r|{"communalTable":{"capacity":42}}|] ~=? actual
  ,
  "Serialize single table for 2" ~:
    let table = trySingleTable 2 1
        actual = serializeTable <$> table
    in Just [r|{"singleTable":{"capacity":2,"minimalReservation":1}}|] ~=? actual
  ,
  "Deserialize communal table for 42" ~:
    let json = [r|{"communalTable":{"capacity":42}}|]
        actual = tryDeserializeTable json
    in tryCommunalTable 42 ~=? actual
  ,
  "Deserialize single table for 4" ~:
    let json = [r|{"singleTable":{"capacity":4,"minimalReservation":3}}|]
        actual = tryDeserializeTable json
    in trySingleTable 4 3 ~=? actual
  ,
  -----------------------------------------------
  -- Reflection-based API and its consequences --
  -----------------------------------------------
  "Deserialize communal table via generics" ~:
    let json = [r|{"communalTable":{"capacity":42}}|]
        actual = decode json
    in Just (TableTDR { communalTable = Just CommunalDTR { communalCapacity = 42 }, singleTable = Nothing }) ~=? actual
  ,
  "Serialize communal table via generics" ~:
    let table = TableTDR { communalTable = Just CommunalDTR { communalCapacity = 42 }, singleTable = Nothing }
        actual = encode table
    in [r|{"communalTable":{"capacity":42}}|] ~=? actual
  ]