{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import Restaurants
import JSONSerialization
import Test.HUnit
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.Framework (defaultMain)

main :: IO ()
main = defaultMain $ hUnitTestToTests $ TestList [
  "Serialize communal table for 42" ~:
    let table = tryCommunalTable 42
        actual = encode . JSONTable <$> table
    in Just "{\"communalTable\":{\"capacity\":42}}" ~=? actual
  ]