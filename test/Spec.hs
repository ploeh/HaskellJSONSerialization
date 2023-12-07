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
    in Just TableDTO {
        communalTable = Just CommunalDTO { communalCapacity = 42 },
        singleTable = Nothing } ~=? actual
  ,
  "Deserialize single table via generics" ~:
    let json = [r|{"singleTable":{"capacity":4,"minimalReservation":3}}|]
        actual = decode json
    in Just TableDTO {
        communalTable = Nothing,
        singleTable =
          Just SingleDTO { singleCapacity = 4, minimalReservation = 3 }
      } ~=? actual
  ,
  "Serialize communal table via generics" ~:
    let table = TableDTO {
          communalTable = Just CommunalDTO { communalCapacity = 42 },
          singleTable = Nothing }
        actual = encode table
    in [r|{"communalTable":{"capacity":42}}|] ~=? actual
  ,
  "Serialize single table via generics" ~:
    let table = TableDTO {
          communalTable = Nothing,
          singleTable = Just SingleDTO {
            singleCapacity = 4, minimalReservation = 3 } }
        actual = encode table
    in [r|{"singleTable":{"capacity":4,"minimalReservation":3}}|] ~=? actual
  ,
  "Try parse communal DTO to Domain Model" ~:
    let dto = TableDTO {
          communalTable = Just CommunalDTO { communalCapacity = 42 },
          singleTable = Nothing }
        actual = tryParseTable dto
    in tryCommunalTable 42 ~=? actual
  ,
  "Try parse single DTO to Domain Model" ~:
    let dto = TableDTO {
          communalTable = Nothing,
          singleTable = Just SingleDTO {
            singleCapacity = 4, minimalReservation = 3 } }
        actual = tryParseTable dto
    in trySingleTable 4 3 ~=? actual
  ,
  "Convert single table to DTO" ~:
    let table = trySingleTable 4 3
        actual = toTableDTO <$> table
    in Just TableDTO {
        communalTable = Nothing,
        singleTable =
          Just SingleDTO { singleCapacity = 4, minimalReservation = 3 }
      } ~=? actual
  ,
  "Convert communal table to DTO" ~:
    let table = tryCommunalTable 42
        actual = toTableDTO <$> table
    in Just TableDTO {
        communalTable = Just CommunalDTO { communalCapacity = 42 },
        singleTable = Nothing } ~=? actual
  ]