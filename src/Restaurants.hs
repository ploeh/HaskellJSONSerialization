{-# LANGUAGE PatternSynonyms #-}
module Restaurants (
  Natural,
  pattern N,
  tryNatural,
  SingleTable,
  pattern SingleT,
  Table(..),
  trySingleTable,
  tryCommunalTable) where

newtype Natural = Natural Integer deriving (Eq, Ord, Show)

{-# COMPLETE N #-}
pattern N :: Integer -> Natural
pattern N i <- Natural i

tryNatural :: Integer -> Maybe Natural
tryNatural n
  | n < 1 = Nothing
  | otherwise = Just (Natural n)

data SingleTable = SingleTable
  { singleCapacity :: Natural
  , minimalReservation :: Natural
  } deriving (Eq, Ord, Show)

{-# COMPLETE SingleT #-}
pattern SingleT :: Natural -> Natural -> SingleTable
pattern SingleT c m <- SingleTable c m

data Table = Single SingleTable | Communal Natural deriving (Eq, Show)

trySingleTable :: Integer -> Integer -> Maybe Table
trySingleTable capacity minimal = do
  c <- tryNatural capacity
  m <- tryNatural minimal
  if c < m then Nothing else Just (Single (SingleTable c m))

tryCommunalTable :: Integer -> Maybe Table
tryCommunalTable = fmap Communal . tryNatural